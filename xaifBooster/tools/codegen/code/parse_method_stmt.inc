ourStartActionCatalogue.addElement(AliasMapEntry::ourXAIFName, ActionItem(&XAIFBaseParserHandlers::onAliasMapEntry));
ourStartActionCatalogue.addElement(AliasMap::ourXAIFName, ActionItem(&XAIFBaseParserHandlers::onAliasMap));
ourStartActionCatalogue.addElement(AliasRange::ourXAIFName, ActionItem(&XAIFBaseParserHandlers::onAliasRange));
ourStartActionCatalogue.addElement(Argument::ourXAIFName, ActionItem(&XAIFBaseParserHandlers::onArgument));
ourStartActionCatalogue.addElement(ArgumentList::ourXAIFName, ActionItem(&XAIFBaseParserHandlers::onArgumentList));
ourStartActionCatalogue.addElement(ArgumentSymbolReference::ourXAIFName, ActionItem(&XAIFBaseParserHandlers::onArgumentSymbolReference));
ourStartActionCatalogue.addElement(ArrayAccess::ourXAIFName, ActionItem(&XAIFBaseParserHandlers::onArrayAccess));
ourStartActionCatalogue.addElement(Assignment::ourXAIFName, ActionItem(&XAIFBaseParserHandlers::onAssignment));
ourStartActionCatalogue.addElement(Assignment::our_myLHS_XAIFName, ActionItem(&XAIFBaseParserHandlers::onAssignmentLHS));
ourStartActionCatalogue.addElement(Assignment::our_myRHS_XAIFName, ActionItem(&XAIFBaseParserHandlers::onAssignmentRHS));
ourStartActionCatalogue.addElement(BasicBlock::ourXAIFName, ActionItem(&XAIFBaseParserHandlers::onBasicBlock));
ourStartActionCatalogue.addElement(BooleanOperation::ourXAIFName, ActionItem(&XAIFBaseParserHandlers::onBooleanOperation));
ourStartActionCatalogue.addElement(IndexTriplet::our_myBound_XAIFName, ActionItem(&XAIFBaseParserHandlers::onBound));
ourStartActionCatalogue.addElement(Branch::ourXAIFName, ActionItem(&XAIFBaseParserHandlers::onBranch));
ourStartActionCatalogue.addElement(CallGraph::ourXAIFName, ActionItem(&XAIFBaseParserHandlers::onCallGraph));
ourStartActionCatalogue.addElement(CallGraphEdge::ourXAIFName, ActionItem(&XAIFBaseParserHandlers::onCallGraphEdge));
ourStartActionCatalogue.addElement(ConcreteArgument::ourXAIFName, ActionItem(&XAIFBaseParserHandlers::onConcreteArgument));
ourStartActionCatalogue.addElement(Condition::ourXAIFName, ActionItem(&XAIFBaseParserHandlers::onCondition));
ourStartActionCatalogue.addElement(Constant::ourXAIFName, ActionItem(&XAIFBaseParserHandlers::onConstant));
ourStartActionCatalogue.addElement(ControlFlowGraph::ourXAIFName, ActionItem(&XAIFBaseParserHandlers::onControlFlowGraph));
ourStartActionCatalogue.addElement(ControlFlowGraphEdge::ourXAIFName, ActionItem(&XAIFBaseParserHandlers::onControlFlowGraphEdge));
ourStartActionCatalogue.addElement(DimensionBounds::ourXAIFName, ActionItem(&XAIFBaseParserHandlers::onDimensionBounds));
ourStartActionCatalogue.addElement(DoMap::ourXAIFName, ActionItem(&XAIFBaseParserHandlers::onDoMap));
ourStartActionCatalogue.addElement(DuUdMap::ourXAIFName, ActionItem(&XAIFBaseParserHandlers::onDuUdMap));
ourStartActionCatalogue.addElement(EndBranch::ourXAIFName, ActionItem(&XAIFBaseParserHandlers::onEndBranch));
ourStartActionCatalogue.addElement(EndLoop::ourXAIFName, ActionItem(&XAIFBaseParserHandlers::onEndLoop));
ourStartActionCatalogue.addElement(Entry::ourXAIFName, ActionItem(&XAIFBaseParserHandlers::onEntry));
ourStartActionCatalogue.addElement(Exit::ourXAIFName, ActionItem(&XAIFBaseParserHandlers::onExit));
ourStartActionCatalogue.addElement(ExpressionEdge::ourXAIFName, ActionItem(&XAIFBaseParserHandlers::onExpressionEdge));
ourStartActionCatalogue.addElement(ForLoop::ourXAIFName, ActionItem(&XAIFBaseParserHandlers::onForLoop));
ourStartActionCatalogue.addElement(Goto::ourXAIFName, ActionItem(&XAIFBaseParserHandlers::onGoto));
ourStartActionCatalogue.addElement(IfStatement::ourXAIFName, ActionItem(&XAIFBaseParserHandlers::onIfStatement));
ourStartActionCatalogue.addElement(IndexTriplet::our_myIndex_XAIFName, ActionItem(&XAIFBaseParserHandlers::onIndex));
ourStartActionCatalogue.addElement(IndexTriplet::ourXAIFName, ActionItem(&XAIFBaseParserHandlers::onIndexTriplet));
ourStartActionCatalogue.addElement(Initialization::ourXAIFName, ActionItem(&XAIFBaseParserHandlers::onInitialization));
ourStartActionCatalogue.addElement(Intrinsic::ourXAIFName, ActionItem(&XAIFBaseParserHandlers::onIntrinsic));
ourStartActionCatalogue.addElement(Label::ourXAIFName, ActionItem(&XAIFBaseParserHandlers::onLabel));
ourStartActionCatalogue.addElement(Marker::ourXAIFName, ActionItem(&XAIFBaseParserHandlers::onMarker));
ourStartActionCatalogue.addElement(SideEffectListType::our_Mod_XAIFName, ActionItem(&XAIFBaseParserHandlers::onMod));
ourStartActionCatalogue.addElement(SideEffectListType::our_ModLocal_XAIFName, ActionItem(&XAIFBaseParserHandlers::onModLocal));
ourStartActionCatalogue.addElement(SideEffectListType::our_OnEntry_XAIFName, ActionItem(&XAIFBaseParserHandlers::onOnEntry));
ourStartActionCatalogue.addElement(PostLoop::ourXAIFName, ActionItem(&XAIFBaseParserHandlers::onPostLoop));
ourStartActionCatalogue.addElement(PreLoop::ourXAIFName, ActionItem(&XAIFBaseParserHandlers::onPreLoop));
ourStartActionCatalogue.addElement(SideEffectListType::our_Read_XAIFName, ActionItem(&XAIFBaseParserHandlers::onRead));
ourStartActionCatalogue.addElement(SideEffectListType::our_ReadLocal_XAIFName, ActionItem(&XAIFBaseParserHandlers::onReadLocal));
ourStartActionCatalogue.addElement(Scope::ourXAIFName, ActionItem(&XAIFBaseParserHandlers::onScope));
ourStartActionCatalogue.addElement(ScopeContainment::ourXAIFName, ActionItem(&XAIFBaseParserHandlers::onScopeContainment));
ourStartActionCatalogue.addElement(Scopes::ourXAIFName, ActionItem(&XAIFBaseParserHandlers::onScopes));
ourStartActionCatalogue.addElement(SideEffectList::our_SideEffectReference_XAIFName, ActionItem(&XAIFBaseParserHandlers::onSideEffectReference));
ourStartActionCatalogue.addElement(StatementIdSetMapEntry::our_StatementId_XAIFName, ActionItem(&XAIFBaseParserHandlers::onStatementId));
ourStartActionCatalogue.addElement(StatementIdSetMapEntry::ourXAIFName, ActionItem(&XAIFBaseParserHandlers::onStatementIdSetMapEntry));
ourStartActionCatalogue.addElement(IndexTriplet::our_myStride_XAIFName, ActionItem(&XAIFBaseParserHandlers::onStride));
ourStartActionCatalogue.addElement(SubroutineCall::ourXAIFName, ActionItem(&XAIFBaseParserHandlers::onSubroutineCall));
ourStartActionCatalogue.addElement(Symbol::ourXAIFName, ActionItem(&XAIFBaseParserHandlers::onSymbol));
ourStartActionCatalogue.addElement(VariableSymbolReference::ourXAIFName, ActionItem(&XAIFBaseParserHandlers::onSymbolReference));
ourStartActionCatalogue.addElement(SymbolReferenceProperty::ourXAIFName, ActionItem(&XAIFBaseParserHandlers::onSymbolReferenceProperty));
ourStartActionCatalogue.addElement(SymbolTable::ourXAIFName, ActionItem(&XAIFBaseParserHandlers::onSymbolTable));
ourStartActionCatalogue.addElement(Update::ourXAIFName, ActionItem(&XAIFBaseParserHandlers::onUpdate));
ourStartActionCatalogue.addElement(VariableEdge::ourXAIFName, ActionItem(&XAIFBaseParserHandlers::onVariableEdge));
ourStartActionCatalogue.addElement("#comment", ActionItem(&XAIFBaseParserHandlers::onDummy));
ourStartActionCatalogue.addElement("#document", ActionItem(&XAIFBaseParserHandlers::onDummy));
ourStartActionCatalogue.addElement("#text", ActionItem(&XAIFBaseParserHandlers::onDummy));