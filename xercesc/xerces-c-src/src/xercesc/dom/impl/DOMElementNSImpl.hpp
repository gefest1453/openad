#ifndef DOMElementNSImpl_HEADER_GUARD_
#define DOMElementNSImpl_HEADER_GUARD_

/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 * 
 *      http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

/*
 * $Id: DOMElementNSImpl.hpp,v 1.1 2008/02/19 19:02:09 utke Exp $
 */

//
//  This file is part of the internal implementation of the C++ XML DOM.
//  It should NOT be included or used directly by application programs.
//
//  Applications should include the file <xercesc/dom/DOM.hpp> for the entire
//  DOM API, or xercesc/dom/DOM*.hpp for individual DOM classes, where the class
//  name is substituded for the *.
//


#include "DOMElementImpl.hpp"

XERCES_CPP_NAMESPACE_BEGIN

class DOMTypeInfoImpl;

class CDOM_EXPORT DOMElementNSImpl: public DOMElementImpl {
protected:
    //Introduced in DOM Level 2
    const XMLCh * fNamespaceURI;     //namespace URI of this node
    const XMLCh * fLocalName;        //local part of qualified name
    const XMLCh * fPrefix;

private:
    const DOMTypeInfoImpl *fSchemaType;

public:
    DOMElementNSImpl(DOMDocument *ownerDoc, const XMLCh *name);
    DOMElementNSImpl(DOMDocument *ownerDoc, //DOM Level 2
	const XMLCh *namespaceURI, const XMLCh *qualifiedName);
    DOMElementNSImpl(const DOMElementNSImpl &other, bool deep=false);

    virtual DOMNode * cloneNode(bool deep) const;
    virtual bool isSupported(const XMLCh *feature, const XMLCh *version) const;
    virtual DOMNode * getInterface(const XMLCh* feature);

    //Introduced in DOM Level 2
    virtual const XMLCh *getNamespaceURI() const;
    virtual const XMLCh *getPrefix() const;
    virtual const XMLCh *getLocalName() const;
    virtual void         setPrefix(const XMLCh *prefix);
    virtual void         release();

    //Introduced in DOM Level 3
    virtual const XMLCh *getBaseURI() const;
    virtual const DOMTypeInfo * getTypeInfo() const;

   // helper function for DOM Level 3 renameNode
   virtual DOMNode* rename(const XMLCh* namespaceURI, const XMLCh* name);
   void setName(const XMLCh* namespaceURI, const XMLCh* name);

    //helper function for DOM Level 3 TypeInfo
    virtual void setTypeInfo(const DOMTypeInfoImpl* typeInfo);

private:
    // -----------------------------------------------------------------------
    // Unimplemented constructors and operators
    // -----------------------------------------------------------------------    
    DOMElementNSImpl & operator = (const DOMElementNSImpl &);
};

XERCES_CPP_NAMESPACE_END

#endif
