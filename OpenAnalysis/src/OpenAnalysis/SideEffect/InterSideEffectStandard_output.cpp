// /!\ ATTENTION:
//
//     THIS IS AN AUTOMATICALLY GENERATED FILE
//     CREATED BY GenOutputTool.
//     DO NOT EDIT THIS FILE DIRECTLY AS IT WILL
//     BE OVERWRITTEN.

#include "InterSideEffectStandard.hpp"

namespace OA { 
    namespace SideEffect { 

        void InterSideEffectStandard::output(OA::IRHandlesIRInterface &ir)
        {
       	   sOutBuild->objStart("InterSideEffectStandard");

           sOutBuild->mapStart("ProcToSideEffectMap", "ProcHandle", "OA_ptr<OA::SideEffect::SideEffectStandard>" );

           std::map<ProcHandle, OA_ptr<OA::SideEffect::SideEffectStandard> >::iterator reg_ProcToSideEffectStandard_iterator;
           for(reg_ProcToSideEffectStandard_iterator = mProcToSideEffectMap.begin();
		        reg_ProcToSideEffectStandard_iterator != mProcToSideEffectMap.end();
		            reg_ProcToSideEffectStandard_iterator++)
           {

	      ProcHandle first = reg_ProcToSideEffectStandard_iterator->first;
              OA_ptr<OA::SideEffect::SideEffectStandard> &second = reg_ProcToSideEffectStandard_iterator->second;
              sOutBuild->mapEntryStart();
	      sOutBuild->mapKeyStart();
              sOutBuild->outputIRHandle(first, ir);
              sOutBuild->mapKeyEnd();
	      sOutBuild->mapValueStart();
              second->output(ir);
              sOutBuild->mapValueEnd();
	      sOutBuild->mapEntryEnd(); 	
	   }
           sOutBuild->mapEnd("ProcToSideEffectMap");



           sOutBuild->mapStart("CallToSideEffectMap", "CallHandle", "OA_ptr<OA::SideEffect::SideEffectStandard>" );

	   std::map<CallHandle, OA_ptr<OA::SideEffect::SideEffectStandard> >::iterator reg_CallToSideEffectStandard_iterator;
           for(reg_CallToSideEffectStandard_iterator = mCallToSideEffectMap.begin();
                               reg_CallToSideEffectStandard_iterator != mCallToSideEffectMap.end();
                                   reg_CallToSideEffectStandard_iterator++)
           {

     		   CallHandle first = reg_CallToSideEffectStandard_iterator->first;
                   OA_ptr<OA::SideEffect::SideEffectStandard> &second = reg_CallToSideEffectStandard_iterator->second;
	           sOutBuild->mapEntryStart();
	           sOutBuild->mapKeyStart();
	           sOutBuild->outputIRHandle(first, ir);
	           sOutBuild->mapKeyEnd();
	           sOutBuild->mapValueStart();
	           second->output(ir);
	           sOutBuild->mapValueEnd();
	           sOutBuild->mapEntryEnd();
	   }
	   sOutBuild->mapEnd("CallToSideEffectMap");
 

        //  mDefaultSideEffect->output(ir);
 	    
	}
    } // end of namespace SideEffect 
} // end of namespace OA 
