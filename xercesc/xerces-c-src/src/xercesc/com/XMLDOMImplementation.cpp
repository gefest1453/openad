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
 * $Id: XMLDOMImplementation.cpp,v 1.1 2008/02/19 19:02:05 utke Exp $
 */

#include "stdafx.h"
#include "xml4com.h"
#include "XMLDOMImplementation.h"

STDMETHODIMP CXMLDOMImplementation::hasFeature(BSTR feature, BSTR ver, VARIANT_BOOL  *pVal)
{
	ATLTRACE(_T("CXMLDOMImplementation::hasFeature\n"));

	if (NULL == pVal)
		return E_POINTER;

	*pVal = VARIANT_FALSE;

	try
	{
		*pVal = (implementation->hasFeature(feature, ver) ? VARIANT_TRUE : VARIANT_FALSE);
	}
	catch(...)
	{
		return E_FAIL;
	}
	

	return S_OK;
}