{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE FlexibleInstances #-}
module Ide.Plugin.Traverse.Literals where
import           Development.IDE.GHC.Compat
import           Development.IDE.GHC.Util   (unsafePrintSDoc)

-- Depending on our traversal path sometimes we have to parse the "body" of an expression or AST node
-- in the context of how we got there. I.E. we could be in "CMD Mode" where we are parsing a HsCmd or
-- in "Expr Mode" where it's an actual expression
class Traverse a where
    traverseTree :: a -> [Literal]

instance Traverse (LHsExpr GhcPs) where
    traverseTree = traverseLExpr

instance Traverse (LHsCmd GhcPs) where
    traverseTree = traverseLCmd

data Literal = Overloaded SrcSpan (HsOverLit GhcPs)
             | NonOverloaded SrcSpan (HsLit GhcPs)
             | NoLocation Literal -- Unused

instance Show Literal where
    show  = \case
      Overloaded ss hol -> "SourceLoc: " <> show ss <> " - " <> overLitToString hol
      NonOverloaded ss hl -> "SourceLoc: " <> show ss <> " - " <> literalToString hl
      NoLocation lit -> "NoLocation ---> " <> show lit

-- | Find all literals in a Parsed Source File
collectLiterals :: ParsedModule -> [Literal]
collectLiterals = concatMap traverseLDecl . hsmodDecls . unLoc . pm_parsed_source

----------------------------------------- DECLARATIONS -----------------------------------------
-- | Find all Literals in a Declaration.
traverseLDecl :: LHsDecl GhcPs -> [Literal]
traverseLDecl (L _ decls) = traverseDecl decls

traverseDecl :: HsDecl GhcPs -> [Literal]
traverseDecl = \case
  TyClD _ tcd          -> traverseTyClDecl tcd
  ValD _ hsBind        -> traverseBind hsBind
  SpliceD _ spliceDecl -> traverseSpliceDecl spliceDecl
  -- don't think this is necessary
  AnnD _ annDecl       -> traverseAnnDecl annDecl
  RuleD _ ruleDecl     -> traverseRuleDecls ruleDecl
  --------------------------------------------------
  _                    -> []

traverseTyClDecl :: TyClDecl GhcPs -> [Literal]
traverseTyClDecl = \case
  ClassDecl{tcdMeths} -> concatMap traverseLBind tcdMeths
  _                   -> []

traverseAnnDecl :: AnnDecl GhcPs -> [Literal]
traverseAnnDecl = \case
  HsAnnotation _ _ _ expr -> traverseLExpr expr
  _                       -> []

traverseSpliceDecl :: SpliceDecl GhcPs -> [Literal]
traverseSpliceDecl = \case
  SpliceDecl _ splice _ -> traverseLSplice splice
  _                     -> []

traverseRuleDecls :: RuleDecls GhcPs -> [Literal]
traverseRuleDecls = \case
  HsRules _ _ ruleDecls -> concatMap traverseLRuleDecl ruleDecls
  _                     -> []

traverseLRuleDecl :: LRuleDecl GhcPs -> [Literal]
traverseLRuleDecl = traverseRuleDecl . unLoc

traverseRuleDecl :: RuleDecl GhcPs -> [Literal]
traverseRuleDecl = \case
  HsRule{..} -> concatMap traverseLExpr [rd_lhs, rd_rhs]
  _          -> []
----------------------------------------------------------------------------------

traverseLBind :: LHsBindLR idL GhcPs -> [Literal]
traverseLBind = traverseBind . unLoc

traverseBind :: HsBindLR idL GhcPs -> [Literal]
traverseBind = \case
    FunBind{fun_matches} -> traverseMatchGroup fun_matches
    VarBind{var_rhs}     -> traverseLExpr var_rhs
    _                    -> []

traverseMatchGroup :: Traverse p => MatchGroup GhcPs p -> [Literal]
traverseMatchGroup = \case
  MG _ matches _ -> concatMap traverseLMatch $ unLoc matches
  _              -> []

traverseLMatch :: Traverse p => LMatch GhcPs p -> [Literal]
traverseLMatch = traverseMatch . unLoc

traverseMatch :: Traverse p => Match GhcPs p -> [Literal]
traverseMatch = \case
  Match{..} -> concatMap traverseLPat m_pats <> traverseGRHSs m_grhss
  _         -> []

traverseLPat :: LPat GhcPs -> [Literal]
traverseLPat (L sSpan pat) = traversePat sSpan pat

traversePat :: SrcSpan -> Pat GhcPs -> [Literal]
traversePat sSpan = \case
      LazyPat _ lpat                           -> traverseLPat lpat
      AsPat _ _ lpat                           -> traverseLPat lpat
      ParPat _ lpat                            -> traverseLPat lpat
      -- TODO: Do we bother with patterns like this? BangPatterns will never have literals
      BangPat _ lpat                           -> traverseLPat lpat
      ListPat _ lpats                          -> concatMap traverseLPat lpats
      TuplePat _ lpats _                       -> concatMap traverseLPat lpats
      SumPat _ lpat _ _                        -> traverseLPat lpat
      -- ConPat replaced these two in at least ghc-9.2.1 lib
      ConPatIn _ details                       -> concatMap traverseLPat ( hsConPatArgs details)
      ConPatOut{pat_args}                      -> concatMap traverseLPat ( hsConPatArgs pat_args)
      -------------------------------------------------------------------
      ViewPat _ expr lpat                      -> traverseLExpr expr <> traverseLPat lpat
      SplicePat _ splice                       -> traverseSplice sSpan splice
      LitPat _ lit                             -> getLiteral sSpan lit
      NPat _ (L olSpan overLit) sexpr1 sexpr2  -> getOverLiteral olSpan overLit
                                                <> maybe [] (traverseSynExpr sSpan) sexpr1
                                                <> traverseSynExpr sSpan sexpr2
      NPlusKPat _ _ (L olSpan loverLit) overLit sexpr1 sexpr2 -> getOverLiteral olSpan loverLit
                                                      <> getOverLiteral sSpan overLit
                                                      <> traverseSynExpr sSpan sexpr1
                                                      <> traverseSynExpr sSpan sexpr2
      SigPat _ lpat _                          -> traverseLPat lpat
      CoPat _ _ pat _                          -> traversePat sSpan pat
      _                                        -> []

traverseGRHSs :: Traverse p => GRHSs GhcPs p -> [Literal]
traverseGRHSs = \case
  GRHSs{..} -> concatMap traverseLGRHS grhssGRHSs <> traverseLLocalBinds grhssLocalBinds
  _         -> []

traverseLGRHS :: Traverse p => LGRHS GhcPs p -> [Literal]
traverseLGRHS = traverseGRHS . unLoc

traverseGRHS :: Traverse p => GRHS GhcPs p -> [Literal]
traverseGRHS = \case
  GRHS _ guards rhs -> traverseTree rhs <> concatMap traverseLStmt guards
  _                 -> []

traverseLLocalBinds :: LHsLocalBinds GhcPs -> [Literal]
traverseLLocalBinds = traverseLocalBinds . unLoc

traverseLocalBinds :: HsLocalBinds GhcPs -> [Literal]
traverseLocalBinds = \case
  HsValBinds _ valBinds -> traverseValBinds valBinds
  _                     -> []

traverseValBinds :: HsValBindsLR idL GhcPs  -> [Literal]
traverseValBinds = \case
  ValBinds _ hsBinds _ -> concatMap traverseLBind hsBinds
  _                    -> []

traverseLStmt :: Traverse p => LStmt GhcPs p -> [Literal]
traverseLStmt (L sSpan stmt) = traverseStmt sSpan stmt

traverseStmt :: Traverse p => SrcSpan -> Stmt GhcPs p -> [Literal]
traverseStmt sSpan = \case
  LastStmt _ expr _ sexpr            -> traverseTree expr <> traverseSynExpr sSpan sexpr
  BindStmt _ lpat expr sexpr1 sexpr2 -> traverseLPat lpat <> traverseTree expr <> concatMap (traverseSynExpr sSpan) [sexpr1, sexpr2]
  ApplicativeStmt _ appPairs sexpr   -> concatMap (traverseAppPair sSpan) appPairs <> maybe [] (traverseSynExpr sSpan) sexpr
  BodyStmt _ expr sexpr1 sexpr2      -> traverseTree expr <> traverseSynExpr sSpan sexpr1 <> traverseSynExpr sSpan sexpr2
  LetStmt _ locBinds                 -> traverseLLocalBinds locBinds
  ParStmt _ parStmts expr sexpr      -> concatMap (traverseParStmtBlock sSpan) parStmts <> traverseExpr sSpan expr <> traverseSynExpr sSpan sexpr
  TransStmt{..}                      -> concatMap traverseLStmt trS_stmts <> traverseLExpr trS_using
                                        <> maybe [] traverseLExpr trS_by <> traverseExpr sSpan trS_fmap
                                        <> concatMap (traverseSynExpr sSpan) [trS_ret, trS_bind]
  RecStmt{..}                        -> concatMap traverseLStmt recS_stmts
                                        <> concatMap (traverseSynExpr sSpan) [recS_bind_fn, recS_ret_fn, recS_mfix_fn]
  _                                  -> []

traverseParStmtBlock :: SrcSpan -> ParStmtBlock GhcPs GhcPs -> [Literal]
traverseParStmtBlock sSpan = \case
  ParStmtBlock _ stmts _ sexpr -> concatMap traverseLStmt stmts <> traverseSynExpr sSpan sexpr
  _ -> []

traverseLExpr :: LHsExpr GhcPs -> [Literal]
traverseLExpr (L sSpan hsExpr) = traverseExpr sSpan hsExpr

traverseExpr :: SrcSpan -> HsExpr GhcPs -> [Literal]
traverseExpr sSpan = \case
      HsOverLit _ overLit             -> [Overloaded sSpan overLit | isNumericOverLit overLit]
      HsLit _ lit                     -> [NonOverloaded sSpan lit | isNumericLiteral lit]
      HsLam _ group                   -> traverseMatchGroup group
      HsLamCase _ group               -> traverseMatchGroup group
      HsApp _ expr1 expr2             -> concatMap traverseLExpr [expr1, expr2]
      HsAppType _ expr _              -> traverseLExpr expr
      OpApp _ expr1 expr2 expr3       -> concatMap traverseLExpr [expr1, expr2, expr3]
      NegApp _ expr _                 -> traverseLExpr expr
      HsPar _ expr                    -> traverseLExpr expr
      SectionL _ expr1 expr2          -> concatMap traverseLExpr [expr1, expr2]
      SectionR _ expr1 expr2          -> concatMap traverseLExpr [expr1, expr2]
      ExplicitTuple _ args _          -> concatMap traverseLTuple args
      ExplicitSum _ _ _ expr          -> traverseLExpr expr
      HsCase _ expr group             -> traverseLExpr expr <> traverseMatchGroup group
      HsIf _ sexpr expr1 expr2 expr3  -> concatMap traverseLExpr [expr1, expr2, expr3] <> maybe [] (traverseSynExpr sSpan) sexpr
      HsMultiIf _ grhss               -> concatMap traverseLGRHS grhss
      HsLet _ locBinds expr           -> traverseLLocalBinds locBinds <> traverseLExpr expr
      HsDo _ _ stmt                   -> concatMap traverseLStmt (unLoc stmt)
      ExplicitList _ sexpr exprs      -> concatMap traverseLExpr exprs <> maybe [] (traverseSynExpr sSpan) sexpr
      RecordCon{rcon_flds}            -> traverseRecordBinds rcon_flds
      RecordUpd{..}                   -> traverseLExpr rupd_expr <> concatMap traverseLRecordUpdate rupd_flds
      ExprWithTySig _ expr _          -> traverseLExpr expr
      ArithSeq _ sexpr seqInfo        -> maybe [] (traverseSynExpr sSpan) sexpr <> traverseArithSeqInfo seqInfo
      HsSCC _ _ _ expr                -> traverseLExpr expr
      HsCoreAnn _ _ _ expr            -> traverseLExpr expr
      HsBracket _ brackets            -> traverseBrackets brackets
      HsSpliceE _ splice              -> traverseSplice sSpan splice
      HsProc _ _ cmdTop               -> traverseLCmdTop cmdTop
      HsStatic _ expr                 -> traverseLExpr expr
      HsTick _ _ expr                 -> traverseLExpr expr
      HsBinTick _ _ _ expr            -> traverseLExpr expr
      HsTickPragma _ _ _ _ expr       -> traverseLExpr expr
      HsWrap _ _ expr                 -> traverseExpr sSpan expr
      _                               -> []

traverseLCmdTop :: LHsCmdTop GhcPs -> [Literal]
traverseLCmdTop = traverseCmdTop . unLoc

traverseCmdTop :: HsCmdTop GhcPs -> [Literal]
traverseCmdTop = \case
  HsCmdTop _ cmds -> traverseLCmd cmds
  _               -> []

traverseLCmd :: LHsCmd GhcPs -> [Literal]
traverseLCmd (L sSpan cmd) = traverseCmd sSpan cmd

traverseCmd :: SrcSpan -> HsCmd GhcPs -> [Literal]
traverseCmd sSpan = \case
  HsCmdArrApp _ expr1 expr2 _ _   -> concatMap traverseLExpr [expr1, expr2]
  HsCmdArrForm _ expr _ _ cmdTops -> traverseLExpr expr <> concatMap traverseLCmdTop cmdTops
  HsCmdApp _ cmd expr             -> traverseLCmd cmd <> traverseLExpr expr
  HsCmdLam _ group                -> traverseMatchGroup group
  HsCmdPar _ cmd                  -> traverseLCmd cmd
  HsCmdCase _ expr group          -> traverseTree expr <> traverseMatchGroup group
  HsCmdIf _ sexpr expr cmd1 cmd2  -> maybe [] (traverseSynExpr sSpan) sexpr <> traverseLExpr expr <> concatMap traverseLCmd [cmd1, cmd2]
  HsCmdLet _ locBinds cmd         -> traverseLLocalBinds locBinds <> traverseLCmd cmd
  HsCmdDo _ cmdStmts              -> concatMap traverseLStmt (unLoc cmdStmts)
  HsCmdWrap _ _ cmd               -> traverseCmd sSpan cmd
  _ -> []

traverseBrackets :: HsBracket GhcPs -> [Literal]
traverseBrackets = \case
  ExpBr _ expr     -> traverseLExpr expr
  PatBr _ lpat     -> traverseLPat lpat
  DecBrL _ decls   -> concatMap traverseLDecl decls
  DecBrG _ hsGroup -> traverseHsGroup hsGroup
  TExpBr _ expr    -> traverseLExpr expr
  _                -> []

traverseHsGroup :: HsGroup GhcPs -> [Literal]
traverseHsGroup = \case
  HsGroup{hs_valds} -> case hs_valds of
    ValBinds _ hsBinds _ -> concatMap traverseLBind hsBinds
    _                    -> []
  _           -> []

traverseArithSeqInfo :: ArithSeqInfo GhcPs -> [Literal]
traverseArithSeqInfo = \case
  From expr                    -> traverseLExpr expr
  FromThen expr1 expr2         -> concatMap traverseLExpr [expr1, expr2]
  FromTo expr1 expr2           -> concatMap traverseLExpr [expr1, expr2]
  FromThenTo expr1 expr2 expr3 -> concatMap traverseLExpr [expr1, expr2, expr3]

traverseLRecordUpdate :: LHsRecUpdField GhcPs -> [Literal]
traverseLRecordUpdate = traverseRecordUpdate . unLoc

traverseRecordUpdate :: HsRecUpdField GhcPs -> [Literal]
traverseRecordUpdate HsRecField{..} = traverseLExpr hsRecFieldArg

traverseRecordBinds :: HsRecordBinds GhcPs -> [Literal]
traverseRecordBinds HsRecFields{..} = concatMap traverseLRecordField rec_flds

traverseLRecordField :: Traverse p => LHsRecField GhcPs p -> [Literal]
traverseLRecordField = traverseRecordField . unLoc

traverseRecordField :: Traverse p => HsRecField GhcPs p -> [Literal]
traverseRecordField HsRecField{..} = traverseTree hsRecFieldArg

traverseAppPair :: SrcSpan -> (SyntaxExpr GhcPs, ApplicativeArg GhcPs) -> [Literal]
traverseAppPair sSpan (sexpr, appArg) = traverseSynExpr sSpan sexpr <> traverseAppArg sSpan appArg

traverseAppArg :: SrcSpan -> ApplicativeArg GhcPs -> [Literal]
traverseAppArg sSpan = \case
  ApplicativeArgOne{..}-> traverseLPat app_arg_pattern <> traverseLExpr arg_expr <> traverseSynExpr sSpan fail_operator
  ApplicativeArgMany{..}-> concatMap traverseLStmt app_stmts <> traverseExpr sSpan final_expr <> traverseLPat bv_pattern
  _ -> []

traverseLTuple :: LHsTupArg GhcPs -> [Literal]
traverseLTuple = traverseTuple . unLoc

traverseTuple :: HsTupArg GhcPs -> [Literal]
traverseTuple = \case
  Present _ expr -> traverseLExpr expr
  _              -> []

traverseLSplice :: Located (HsSplice GhcPs) -> [Literal]
traverseLSplice (L sSpan splice) = traverseSplice sSpan splice

traverseSplice :: SrcSpan -> HsSplice GhcPs -> [Literal]
traverseSplice sSpan = \case
  HsTypedSplice _ _ _ expr   -> traverseLExpr expr
  HsUntypedSplice _ _ _ expr -> traverseLExpr expr
  HsSpliced _  _ st          -> case st of
    HsSplicedExpr expr -> traverseExpr sSpan expr
    HsSplicedPat pat   -> traversePat sSpan pat
    _                  ->[]
  _                          -> []

traverseSynExpr :: SrcSpan -> SyntaxExpr GhcPs -> [Literal]
traverseSynExpr sSpan SyntaxExpr{syn_expr} = traverseExpr sSpan syn_expr

-- Translate from Hs Type to our Literal type
getLiteral :: SrcSpan -> HsLit GhcPs -> [Literal]
getLiteral sSpan lit = [NonOverloaded sSpan lit | isNumericLiteral lit]

getOverLiteral :: SrcSpan -> HsOverLit GhcPs -> [Literal]
getOverLiteral sSpan lit = [Overloaded sSpan lit | isNumericOverLit lit]

-- Ignore non-numerics
isNumericLiteral :: HsLit p -> Bool
isNumericLiteral = \case
    HsChar _ _       -> False
    HsCharPrim _ _   -> False
    HsString _ _     -> False
    HsStringPrim _ _ -> False
    _                -> True

isNumericOverLit :: HsOverLit p -> Bool
isNumericOverLit = \case
    OverLit{ol_val} -> isNumericOverLit' ol_val
    _               -> False

-- only a single non-numeric literal
isNumericOverLit' :: OverLitVal -> Bool
isNumericOverLit' = \case
    HsIsString _ _ -> False
    _              -> True

-- mostly for debugging purposes
literalToString :: HsLit p -> String
literalToString = \case
  HsChar _ c        -> "Char: " <> show c
  HsCharPrim _ c    -> "CharPrim: " <> show c
  HsString _ fs     -> "String: " <> show fs
  HsStringPrim _ bs -> "StringPrim: " <> show bs
  HsInt _ il        -> "Int: " <> show il
  HsIntPrim _ n     -> "IntPrim: " <> show n
  HsWordPrim _ n    -> "WordPrim: " <> show n
  HsInt64Prim _ n   -> "Int64Prim: " <> show n
  HsWord64Prim _ n  -> "Word64Prim: " <> show n
  HsInteger _ n ty  -> "Integer: " <> show n <> " Type: " <> tyToLiteral ty
  HsRat _ fl ty     -> "Rat: " <> show fl <> " Type: " <> tyToLiteral ty
  HsFloatPrim _ fl  -> "FloatPrim: " <> show fl
  HsDoublePrim _ fl -> "DoublePrim: " <>  show fl
  _                 -> "XHsLit"
  where
    tyToLiteral :: Type -> String
    tyToLiteral = unsafePrintSDoc .  ppr

overLitToString :: HsOverLit GhcPs -> String
overLitToString = \case
  OverLit _ overLit _ -> case overLit of
     HsIntegral int -> case int of { IL _ _ val -> "IntegralOverLit: " <> show val }
     HsFractional frac -> case frac of { FL _ _ val -> "RationalOverLit: " <> show val }
     HsIsString _ str -> "HIsString: " <> show str
  _ -> "XOverLit"
