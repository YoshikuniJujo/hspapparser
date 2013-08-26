{-# LANGUAGE QuasiQuotes, TypeFamilies, PackageImports #-}

module Language.Haskell.PapParser (
	haskellSrc,
	haskell
) where

import Prelude hiding (exp, pred)
import Text.Papillon
import Language.Haskell.TH hiding (match, cxt, strictType, Pragma)
import Language.Haskell.TH.Quote
import "monads-tf" Control.Monad.State
import "monads-tf" Control.Monad.Error

import Data.Char
import Data.Maybe
import Data.List
import Control.Applicative
import Control.Arrow

data DecEnv = Normal | Class | Instance deriving (Eq, Show)

type ParseM = State ([Int], DecEnv)
initStat = ([], Normal)

pushX :: Int -> ParseM Bool
pushX x = modify (first (x :)) >> return True

popX :: ParseM Bool
popX = modify (first tail) >> return True

setEnv :: DecEnv -> ParseM Bool
setEnv e = modify (second $ const e) >> return True

getEnv :: ParseM DecEnv
getEnv = gets snd

haskell :: QuasiQuoter
haskell = QuasiQuoter {
	quoteExp = return . haskellExp,
	quotePat = return . haskellPat,
	quoteType = return . haskellTyp,
	quoteDec = return . haskellDec
 }

haskellSrc :: String -> HsSrc
haskellSrc src = case flip evalState initStat $ runErrorT $ hssrc $ parse src of
	Right (p, _) -> p
	Left _ -> error "parse error"

haskellExp :: String -> Exp
haskellExp src = case flip evalState initStat $ runErrorT $ expA $ parse src of
	Right (p, _) -> p
	Left _ -> error "parse error"

haskellPat :: String -> Pat
haskellPat src = case flip evalState initStat $ runErrorT $ patA $ parse src of
	Right (p, _) -> p
	Left _ -> error "parse error"

haskellTyp :: String -> Type
haskellTyp src = case flip evalState initStat $ runErrorT $ typA $ parse src of
	Right (p, _) -> p
	Left _ -> error "parse error"

haskellDec :: String -> [Dec]
haskellDec src = case flip evalState ([1], Normal) $ runErrorT $ decsA $ parse src of
	Right (p, _) -> p
	Left _ -> error "parse error"

testTkn :: String -> Tkn
testTkn src = case flip evalState initStat $ runErrorT $ tkn $ parse src of
	Right (p, _) -> p
	Left _ -> error "parse error"

testLit :: String -> Lit
testLit src = case flip evalState initStat $ runErrorT $ lit $ parse src of
	Right (p, _) -> p
	Left _ -> error "parse error"

isLower_ :: Char -> Bool
isLower_ = (||) <$> isLower <*> (`elem` "_")

isVar :: Char -> Bool
isVar = (||) <$> isAlphaNum <*> (`elem` "'_")

isOp :: Char -> Bool
isOp = (`elem` "!#$%&*+./<=>?@\\^|-~")

data HsSrc = HsSrc {
	srcPragmas :: [Pragma],
	srcModule :: Module,
	srcImports :: [Import],
	srcDecs :: [Dec]
 } deriving Show

data Pragma
	= LanguagePragma [String]
	| OtherPragma String
	deriving Show

data Module = Module {
	modName :: String,
	exportList :: Maybe [Export]
 } deriving Show

data Import = Import {
	packageName :: Maybe String,
	isQualified :: Bool,
	impModName ::  String,
	asName :: Maybe String,
	isHiding :: Bool,
	importList :: Maybe [Export]
 } deriving Show

data Export
	= EMem Member
	| EType String (Maybe [Member])
	deriving Show

data Member
	= MVar String
	| MOp String
	deriving Show

data Tkn
	= TVar String
	| TCon String
	| TOpCon String
	| TOp String
	| TLit Lit
	| TOParen
	| TCParen
	| TOBrace
	| TCBrace
	| TVCBrace
	| TOBracket
	| TCBracket
	| TComma
	| TTilde
	| TAt
	| TEq
	| TBackquote
	| TTypeDef
	| TRightArrow
	| TLeftArrow
	| TDRightArrow
	| TDotDot
	| TForall
	| TBackslash
	| TVBar
	| TIf
	| TThen
	| TElse
	| TLet
	| TIn
	| TCase
	| TOf
	| TDo
	| TWhere
	| TData
	| TNewtype
	| TType
	| TDeriving
	| TClass
	| TInstance
	| TInfix
	| TInfixl
	| TInfixr
	| TImport
	| TQualified
	| TAs
	| THiding
	| TModule
	| TSemicolon
	| TOPragma
	| TCPragma
	deriving Show

putForall :: Type -> Type
putForall t = case typeVars t of
	[] -> t
	vs -> ForallT (map PlainTV vs) [] t

typeVars :: Type -> [Name]
typeVars (ForallT tvb _ t) = typeVars t \\ map (\(PlainTV v) -> v) tvb
typeVars (AppT t1 t2) = typeVars t1 `union` typeVars t2
typeVars (VarT v) = [v]
typeVars _ = []

cxtToTVBs :: Cxt -> [TyVarBndr]
cxtToTVBs = map PlainTV . foldr1 union . map predVars

predVars :: Pred -> [Name]
predVars (ClassP _ ts) = foldr1 union $ map typeVars ts
predVars (EqualP t1 t2) = typeVars t1 `union` typeVars t2

mkTupSec :: [Maybe Exp] -> Exp
mkTupSec ms = LamE ps $ TupE $ mkEs vs ms
	where
	vs = map (mkName . ('_' :) . show) $
		take (length $ filter isNothing ms) [0 .. ]
	ps = map VarP vs
	mkEs [] [] = []
	mkEs vs (Just e : es) = e : mkEs vs es
	mkEs (v : vs) (Nothing : es) = VarE v : mkEs vs es

mkKindVars :: [()] -> [Name]
mkKindVars = map (mkName . ('_' :) . show) . zipWith (flip const) [1 ..]

[papillon|

monad: ParseM

hssrc :: HsSrc
	= ps:pragma* m:mdl &(!(_, x)):lxp[pushX x] is:imprts ds:decsA[popX]
						{ return $ HsSrc ps m is ds }
	/ ps:pragma* m:mdl _:space* !_		{ return $ HsSrc ps m [] [] }

pragma :: Pragma
	= p:languagePragma			{ return p }
	/ TOPragma:lx s:pragmaStr TCPragma:lx	{ return $ OtherPragma s }

pragmaStr :: String
	= !TCPragma:lx c s:pragmaStr		{ return $ c : s }
	/					{ return "" }

languagePragma :: Pragma
	= TOPragma:lx (TCon "LANGUAGE"):lx
		(TCon c0):lx cs:(TComma:lx (TCon c):lx { return c })*
		TCPragma:lx			{ return $ LanguagePragma $ c0 : cs }

mdl :: Module
	= TModule:lx (TCon mod):lx mes:exports? TWhere:lx
						{ return $ Module mod mes }
	/					{ return $ Module "Main" Nothing }

imprts :: [Import]
	= md:imprt? TSemicolon:lx ds:imprts	{ return $ maybe ds (: ds) md }
	/ md:imprt? _:(_:space / '\n')+ &(!(_, x)):lxp[gets $ (== x) . head . fst]
		ds:imprts			{ return $ maybe ds (: ds) md }
	/ md:imprt? _:(_:space / '\n')+ !_	{ return $ maybeToList md }
	/ md:imprt?				{ return $ maybeToList md }

imprt :: Import
	= TImport:lx mp:((TLit (StringL p)):lx { return p })? mq:(TQualified:lx)?
		(TCon mod):lx masn:(TAs:lx (TCon asn):lx { return asn })?
		mh:(THiding:lx)? mes:exports?
						{ return $ Import mp (isJust mq)
							mod masn (isJust mh) mes }

exports :: [Export]
	= TOParen:lx
		mes:(e0:export es:(TComma:lx e:export { return e })*
			{ return $ e0 : es })?
		TCParen:lx
						{ return $ fromMaybe [] mes }

export :: Export
	= (TVar v):lx				{ return $ EMem $ MVar v }
	/ TOParen:lx
		o:((TOp po):lx { return po } / (TOpCon co):lx { return co })
		TCParen:lx			{ return $ EMem $ MOp o }
	/ (TCon c):lx mms:(TOParen:lx mms':members?
		TCParen:lx { return $ fromMaybe [] mms' })?
						{ return $ EType c mms }

members :: [Member] = m0:member ms:(TComma:lx m:member { return m } )*
						{ return $ m0 : ms }

member :: Member
	= (TCon c):lx				{ return $ MVar c }
	/ (TVar v):lx				{ return $ MVar v }
	/ TOParen:lx (TOpCon o):lx TCParen:lx	{ return $ MOp o }

expA :: Exp = e:exp _:space* !_			{ return e }

exp :: Exp = e:expInfix				{ return e }

expInfix :: Exp =
	lft:expPrefix ors:(o:operator r:expPrefix { return (VarE $ mkName o, r) })*
						{ return $ foldl
							(\l (o, r) -> UInfixE l o r)
							lft ors }

operator :: String
	= (TOp o):lx				{ return o }
	/ (TOpCon o):lx				{ return o }
	/ TBackquote:lx (TVar v):lx TBackquote:lx
						{ return v }

expPrefix :: Exp
	= m:((TOp "-"):lx)? e:expApp		{ return $ maybe e
							(const $ VarE (mkName "-")
								`AppE` e) m }

expApp :: Exp = f:expSig as:expSig*		{ return $ foldl AppE f as }

expSig :: Exp = e:expRec mt:(TTypeDef:lx t:typ { return $ putForall t })?
						{ return $ maybe e (SigE e) mt }

expRec :: Exp = e:exp1 mfes:(TOBrace:lx fes:fieldExps TCBrace:lx { return fes })?
						{ return $ maybe e (RecUpdE e) mfes }

exp1 :: Exp
	= (TVar v):lx				{ return $ VarE $ mkName v }
	/ (TCon c):lx TOBrace:lx fes:fieldExps TCBrace:lx
						{ return $ RecConE (mkName c) fes }
	/ (TCon c):lx				{ return $ ConE $ mkName c }
	/ (TLit l):lx				{ return $ LitE l }
	/ !_:(TOParen:lx !_:expApp TCParen:lx)
		TOParen:lx l:exp?
		oo:((TOp o):lx { return o } / (TOpCon o): lx { return o })
		r:exp? TCParen:lx
						{ return $ InfixE l
							(VarE $ mkName oo) r }
	/ TOParen:lx TCParen:lx			{ return $ TupE [] }
	/ TOParen:lx e:exp TCParen:lx		{ return e }
	/ TOParen:lx me0:exp? mes:(TComma:lx me:exp? { return me })+ TCParen:lx
						{ return $ mkTupSec $ me0 : mes }
	/ TBackslash:lx ps:pat1+ TRightArrow:lx e:exp
						{ return $ LamE ps e }
	/ TIf:lx p:exp TThen:lx t:exp TElse:lx e:exp
						{ return $ CondE p t e }
	/ TLet:lx ds:decs' TIn:lx e:exp		{ return $ LetE ds e }
	/ TCase:lx e:exp TOf:lx TOBrace:lx ms:matches TCBrace:lx
						{ return $ CaseE e ms }
	/ TCase:lx e:exp TOf:lx &(!(_, x)):lxp[pushX x] ms:matches[popX]
						{ return $ CaseE e ms }
	/ TDo:lx TOBrace:lx ss:stmts TCBrace:lx	{ return $ DoE ss }
	/ TDo:lx &(!(_, x)):lxp[pushX x] ss:stmts[popX]
						{ return $ DoE ss }
	/ TOBracket:lx r:exp TVBar:lx s0:stmt ss:(TComma:lx s:stmt { return s })*
		TCBracket:lx			{ return $ CompE $ ss ++ [NoBindS r] }
	/ TOBracket:lx TCBracket:lx		{ return $ ListE [] }
	/ TOBracket:lx e0:exp es:(TComma:lx e:exp { return e })* TCBracket:lx
						{ return $ ListE $ e0 : es }
	/ TOBracket:lx r:range TCBracket:lx	{ return $ ArithSeqE r }

range :: Range
	= e1:exp TComma:lx e2:exp TDotDot:lx en:exp
						{ return $ FromThenToR e1 e2 en }
	/ e1:exp TComma:lx e2:exp TDotDot:lx	{ return $ FromThenR e1 e2 }
	/ e1:exp TDotDot:lx en:exp		{ return $ FromToR e1 en }
	/ e:exp TDotDot:lx			{ return $ FromR e }

patA :: Pat = p:pat _:space* !_			{ return p }

pat :: Pat = p:patOp				{ return p }

patOp :: Pat
	= p:pat1 mps:((TOpCon o):lx ps:patOp { return (mkName o, ps) })?
						{ return $ maybe p
							(\(o, ps) -> UInfixP p o ps)
							mps }

pat1 :: Pat
	= (TLit l):lx				{ return $ LitP l }
	/ !(TVar "_"):lx (TVar v):lx !TAt:lx	{ return $ VarP $ mkName v }
	/ TOParen:lx
		mps:(p0:pat ps:(TComma:lx p:pat { return p })*
			{ return $ p0 : ps })?
		TCParen:lx			{ return $ TupP $ fromMaybe [] mps }
	/ (TCon c):lx ps:pat*			{ return $ ConP (mkName c) ps }
	/ TTilde:lx p:pat			{ return $ TildeP p }
	/ (TOp "!"):lx p:pat			{ return $ BangP p }
	/ (TVar v):lx TAt:lx p:pat		{ return $ AsP (mkName v) p }
	/ (TVar "_"):lx				{ return WildP }
	/ (TCon c):lx TOBrace:lx mfps:fieldPats? TCBrace:lx
						{ return $ RecP (mkName c) $
							fromMaybe [] mfps }
	/ TOBracket:lx
		mps:(p0:pat ps:(TComma:lx p:pat { return p })*
			{ return $ p0 : ps })?
		TCBracket:lx			{ return $ ListP $ fromMaybe [] mps }
	/ TOParen:lx p:pat TCParen:lx		{ return p }

typA :: Type = t:typ _:space* !_		{ return $ putForall t }

typ :: Type = t:typSig				{ return t }

typSig :: Type
	= t:typCxt mk:(TTypeDef:lx k:typ { return k })?
						{ return $ maybe t (SigT t) mk }

typCxt :: Type
	= mc:(c:cxt TDRightArrow:lx { return c })? t:typInf
						{ return $ maybe t (\c ->
							ForallT (cxtToTVBs c) c t)
							mc }

typInf :: Type
	= t0:typApp ts:(TRightArrow:lx t:typApp { return t })*
						{ return $ foldl (\x y ->
							ArrowT `AppT` x `AppT` y)
							t0 ts }

typApp :: Type
	= t:typ1 ts:typ1*			{ return $ foldl AppT t ts }

typ1 :: Type
	= TForall:lx tvs:tyVarBndr+ (TOp "."):lx
		mc:(c:cxt TDRightArrow:lx { return c })? t:typ
						{ return $ ForallT tvs
							(fromMaybe [] mc) t }
	/ (TVar v):lx				{ return $ VarT $ mkName v }
	/ (TCon c):lx				{ return $ ConT $ mkName c }
	/ TOParen:lx TCParen:lx			{ return $ TupleT 0 }
	/ TOParen:lx ts:typTup TCParen:lx	{ return $ foldl AppT
							(TupleT $ length ts) ts }
	/ TOParen:lx TRightArrow:lx TCParen:lx	{ return ArrowT }
	/ TOBracket:lx t:typ TCBracket:lx	{ return $ AppT ListT t }
	/ (TOp "*"):lx				{ return StarT }

typTup :: [Type] = t0:typ ts:(TComma:lx t:typ { return t })*
						{ return $ t0 : ts }

cxt :: [Pred]
	= p:pred				{ return [p] }
	/ TOParen:lx p0:pred ps:(TComma:lx p:pred { return p })* TCParen:lx
						{ return $ p0 : ps }

pred :: Pred
	= (TCon c):lx t:typ			{ return $ ClassP (mkName c) [t] }
	/ (TCon c):lx TOParen:lx t0:typ ts:(TComma:lx t:typ { return t })*
		TOParen:lx			{ return $ ClassP (mkName c) $
							t0 : ts }
	/ t1:typInf TTilde:lx t2:typInf		{ return $ EqualP t1 t2 }

decsA :: [Dec] = ds:decs _:space* !_		{ return ds }

decs' :: [Dec]
	= TOBrace:lx ds:decs TCBrace:lx		{ return ds }
	/ &_:(_:('\n' / _:space)* (!(_, x)):lxp[gets $ maybe False (x <=) . listToMaybe . fst])
						{ return [] }
	/ &_:(_:('\n' / _:space)* !_:lx)	{ return [] }
	/ &(!(_, x)):lxp[pushX x] ds:decs[popX]	{ return ds }

decs :: [Dec]
	= ds:decs_				{ return $ concat ds }

decs_ :: [[Dec]]
	= md:dec? TSemicolon:lx ds:decs_	{ return $ maybe ds (: ds) md }
	/ md:dec? _:(_:space / '\n')+ &(!(_, x)):lxp[gets $ (== x) . head . fst]
		ds:decs_
						{ return $ maybe ds (: ds) md }
	/ md:dec? _:(_:space / '\n')+ !_	{ return $ maybeToList md }
	/ md:dec?				{ return $ maybeToList md }
	/ !_:lx					{ return [] }

dec :: [Dec]
	= d:dec1				{ return [d] }
	/ (TVar v0):lx vs:(TComma:lx (TVar v):lx { return v })* TTypeDef:lx t:typ
						{ return $ map (flip SigD t .
							mkName) $ v0 : vs }
	/ TInfix:lx
		(TLit (IntegerL n)):lx (TOp o0):lx
		os:(TComma:lx (TOp o):lx { return o })*{ return $ map (InfixD $
							Fixity (fromIntegral n)
								InfixN) $
								map mkName $
								o0 : os }
	/ TInfixl:lx
		(TLit (IntegerL n)):lx (TOp o0):lx
		os:(TComma:lx (TOp o):lx { return o })*{ return $ map (InfixD $
							Fixity (fromIntegral n)
								InfixL) $
								map mkName $
								o0 : os }
	/ TInfixr:lx
		(TLit (IntegerL n)):lx (TOp o0):lx
		os:(TComma:lx (TOp o):lx { return o })*{ return $ map (InfixD $
							Fixity (fromIntegral n)
								InfixR) $
								map mkName $
								o0 : os }

dec1 :: Dec
	= p:pat b:body w:whr?			{ return $ ValD p b $
							fromMaybe [] w }
	/ (TVar v0):lx c0:cls
		mcs:(_:(TSemicolon:lx / '\n'+ &(!(_, x)):lxp[gets $ (== x) . head . fst])+
			(TVar v):lx[return $ v == v0] c:cls { return c })*
						{ return $ FunD (mkName v0) $
							c0 : mcs }
	/ TOParen:lx (TOp v0):lx TCParen:lx c0:cls
		mcs:(_:(TSemicolon:lx / '\n'+ &(!(_, x)):lxp[gets $ (== x) . head . fst])+
			TOParen:lx (TOp v):lx[return $ v == v0] TCParen:lx
			c:cls { return c })*
						{ return $ FunD (mkName v0) $
							c0 : mcs }
	/ l0:pat o0:operator r0:pat b0:body w0:whr?
		mcs:(_:(TSemicolon:lx / '\n'+ &(!(_, x)):lxp[gets $ (== x) . head . fst])+
			l:pat o:operator[return $ o == o0] r:pat
			b:body w:whr? { return $ Clause [l, r] b $
				fromMaybe [] w })*
		{ return $ FunD (mkName o0) $ Clause [l0, r0] b0 (fromMaybe [] w0) :
			mcs }
		
	/ ff:(TData:lx { return DataFam } / TType:lx { return TypeFam })
		[(== Class) <$> getEnv]
		(TCon c):lx vs:((TVar v):lx { return $ mkName v })*
						{ return $ FamilyD ff
							(mkName c)
							(map PlainTV vs)
							Nothing }
	/ TData:lx[(== Instance) <$> getEnv]
		mc:(c:cxt TDRightArrow:lx { return c })?
		(TCon c):lx ts:(t:typ { return t })*
		TEq:lx c0:cons cs:(TVBar:lx c:cons { return c })* md:drvng?
						{ return $ DataInstD
							(fromMaybe [] mc)
							(mkName c) ts
							(c0 : cs)
							(fromMaybe [] md) }
	/ TData:lx
		mc:(c:cxt TDRightArrow:lx { return c })?
		(TCon c):lx vs:((TVar v):lx { return $ mkName v })*
		TEq:lx c0:cons cs:(TVBar:lx c:cons { return c })* md:drvng?
						{ return $ DataD
							(fromMaybe [] mc)
							(mkName c)
							(map PlainTV vs)
							(c0 : cs)
							(fromMaybe [] md) }
	/ TData:lx
		mc:(c:cxt TDRightArrow:lx { return c })?
		(TCon c):lx vs:((TVar v):lx { return $ mkName v })*
		TWhere:lx cs:gadtConss' md:drvng?
						{ return $ DataD
							(fromMaybe [] mc)
							(mkName c)
							(map PlainTV vs)
							(map ($ vs) cs)
							(fromMaybe [] md) }
	/ TData:lx
		mc:(c:cxt TDRightArrow:lx { return c })?
		(TCon c):lx TTypeDef: lx
		ss:((TOp "*"):lx TRightArrow:lx)* (TOp "*"):lx
		TWhere:lx cs:gadtConss' md:drvng?
						{ return $ DataD
							(fromMaybe [] mc)
							(mkName c)
							(map PlainTV $ mkKindVars ss)
							(map ($ mkKindVars ss) cs)
							(fromMaybe [] md) }
	/ TNewtype:lx[(== Instance) <$> getEnv]
		mc:(c:cxt TDRightArrow:lx { return c })?
		(TCon c):lx ts:(t:typ { return t })*
		TEq:lx cn:cons md:drvng?	{ return $ NewtypeInstD
							(fromMaybe [] mc)
							(mkName c) ts cn
							(fromMaybe [] md) }
	/ TNewtype:lx
		mc:(c:cxt TDRightArrow:lx { return c })?
		(TCon c):lx vs:((TVar v):lx { return $ mkName v })*
		TEq:lx cn:cons md:drvng?	{ return $ NewtypeD
							(fromMaybe [] mc)
							(mkName c)
							(map PlainTV vs)
							cn
							(fromMaybe [] md) }
	/ TType:lx[(== Instance) <$> getEnv]
		(TCon c):lx ts:(t:typ { return t })*
		TEq:lx t:typ			{ return $ TySynInstD
							(mkName c) ts t }
	/ TType:lx
		(TCon c):lx vs:((TVar v):lx { return $ mkName v })*
		TEq:lx t:typ			{ return $ TySynD
							(mkName c)
							(map PlainTV vs)
							t }
	/ TClass:lx mc:(c:cxt TDRightArrow:lx { return c })?
		(TCon c):lx vs:((TVar v):lx { return $ mkName v })*
		TWhere:lx[setEnv Class] ds:decs'[setEnv Normal]
						{ return $ ClassD
							(fromMaybe [] mc)
							(mkName c)
							(map PlainTV vs) [] ds }
	/ TInstance:lx mc:(c:cxt TDRightArrow:lx { return c })?
		t:typ TWhere:lx[setEnv Instance] ds:decs'[setEnv Normal]
						{ return $ InstanceD
							(fromMaybe [] mc)
							t ds }

cons :: Con
	= (TCon c):lx TOBrace:lx
		vts:((TVar v):lx TTypeDef:lx t:typ { return (mkName v, NotStrict, t) })*
		TCBrace:lx			{ return $ RecC (mkName c) vts }
	/ (TCon c):lx ts:(t:strictType { return t })*
						{ return $ NormalC (mkName c) ts }

gadtConss' :: [[Name] -> Con]
	= TOBrace:lx cs:gadtConss TCBrace:lx	{ return cs }
	/ &_:(_:('\n' / _:space)* (!(_, x)):lxp[gets $
		maybe False (x <=) . listToMaybe . fst])
						{ return [] }
	/ &_:(_:('\n' / _:space)* !_:lx)	{ return [] }
	/ &(!(_, x)):lxp[pushX x] cs:gadtConss[popX]
						{ return cs }

gadtConss :: [[Name] -> Con]
	= mc:gadtCons? TSemicolon:lx cs:gadtConss
						{ return $ maybe cs (: cs) mc }
	/ mc:gadtCons? _:(_:space / '\n')+ &(!(_, x)):lxp
		[gets $ (== x) . head . fst] cs:gadtConss
						{ return $ maybe cs (: cs) mc }
	/ mc:gadtCons? _:(_:space / '\n')+ !_	{ return $ maybeToList mc }
	/ mc:gadtCons?				{ return $ maybeToList mc }

gadtCons :: ([Name] -> Con)
	= (TCon c):lx TTypeDef:lx
		mc:(c:cxt TDRightArrow:lx { return c })?
		ts:(t:typApp TRightArrow:lx { return t })*
		(TCon _):lx xs:typ1*
		{ return (\ns -> ForallC
				(maybe [] cxtToTVBs mc)
				(fromMaybe [] mc ++ zipWith (EqualP . VarT) ns xs) $
			NormalC (mkName c) $ map (\t -> (NotStrict, t)) ts) }
--		{ return (\ns -> undefined) }

{-
typCxt :: Type
	= mc:(c:cxt TDRightArrow:lx { return c })? t:typInf
						{ return $ maybe t (\c ->
							ForallT (cxtToTVBs c) c t)
							mc }
							-}

strictType :: (Strict, Type)
	= (TOp "!"):lx t:typ			{ return (IsStrict, t) }
	/ t:typ					{ return (NotStrict, t) }

drvng :: [Name]
	= TDeriving:lx (TCon c):lx		{ return [mkName c] }
	/ TDeriving:lx TOParen:lx (TCon c0):lx
		cs:(TComma:lx (TCon c):lx { return $ mkName c })* TCParen:lx
						{ return $ mkName c0 : cs }

cls :: Clause
	= ps:pat1* b:body w:whr?		{ return $ Clause ps b $
							fromMaybe [] w }

body :: Body
	= TEq:lx e:exp				{ return $ NormalB e }
	/ gs:(TVBar:lx g:grd TEq:lx e:exp { return (g, e) })+
						{ return $ GuardedB gs }

body' :: Body
	= TRightArrow:lx e:exp			{ return $ NormalB e }
	/ gs:(TVBar:lx g:grd TRightArrow:lx e:exp { return (g, e) })+
						{ return $ GuardedB gs }

grd :: Guard
	= s0:stmt ss:(TComma:lx s:stmt { return s })*
						{ return $ PatG $ s0 : ss }
	/ e:exp					{ return $ NormalG e }

whr :: [Dec]
	= TWhere:lx TOBrace:lx ds:decs TCBrace:lx
						{ return ds }
	/ TWhere:lx &(!(_, x)):lxp[pushX x] ds:decs[popX]
						{ return ds }

matches :: [Match]
	= mm:match? TSemicolon:lx ms:matches	{ return $ maybe ms (: ms) mm }
	/ mm:match? '\n'+ &(!(_, x)):lxp[gets $ (== x) . head . fst] ms:matches
						{ return $ maybe ms (: ms) mm }
	/ mm:match?				{ return $ maybeToList mm }

match :: Match
	= p:pat b:body' w:whr?	{ return $ Match p b $
							fromMaybe [] w }

stmts :: [Stmt]
	= ms:stmt? TSemicolon:lx ss:stmts	{ return $ maybe ss (: ss) ms }
	/ ms:stmt? '\n'+ &(!(_, x)):lxp[gets $ (== x) . head . fst] ss:stmts
						{ return $ maybe ss (: ss) ms }
	/ ms:stmt?				{ return $ maybeToList ms }

stmt :: Stmt
	= p:pat TLeftArrow:lx e:exp		{ return $ BindS p e }
	/ e:exp					{ return $ NoBindS e }
	/ TLet:lx ds:decs'			{ return $ LetS ds }

fieldExps :: [FieldExp] = fe:fieldE fes:(TComma:lx fe:fieldE { return fe })*
						{ return $ fe : fes }

fieldE :: FieldExp = (TVar v):lx TEq:lx e:exp	{ return (mkName v, e) }
						

fieldPats :: [FieldPat] = fp:fieldPt fps:(TComma:lx fp:fieldPt { return fp })*
						{ return $ fp : fps }

fieldPt :: FieldPat = (TVar v):lx TEq:lx p:pat	{ return (mkName v, p) }

tyVarBndr :: TyVarBndr
	= (TVar v):lx				{ return $ PlainTV $ mkName v }
--	/ (TVar v):lx TTypeDef:lx t:typ		{ return $ KindedTV (mkName v) t }

lx :: Tkn = (!(t, _)):lxp			{ return t }

lxp :: (Tkn, Int) = _:space* (!ListPos (CharPos (_, x))):position t:tkn
						{ return (t, x) }

tkn :: Tkn
	= 'f' 'o' 'r' 'a' 'l' 'l' !_:<isVar>	{ return TForall }
	/ 'i' 'f' !_:<isVar>			{ return TIf }
	/ 't' 'h' 'e' 'n' !_:<isVar>		{ return TThen }
	/ 'e' 'l' 's' 'e' !_:<isVar>		{ return TElse }
	/ 'l' 'e' 't' !_:<isVar>		{ return TLet }
	/ 'i' 'n' !_:<isVar>			{ return TIn }
	/ 'c' 'a' 's' 'e' !_:<isVar>		{ return TCase }
	/ 'o' 'f' !_:<isVar>			{ return TOf }
	/ 'd' 'o' !_:<isVar>			{ return TDo }
	/ 'w' 'h' 'e' 'r' 'e' !_:<isVar>	{ return TWhere }
	/ 'd' 'a' 't' 'a' !_:<isVar>		{ return TData }
	/ 'n' 'e' 'w' 't' 'y' 'p' 'e' !_:<isVar>{ return TNewtype }
	/ 't' 'y' 'p' 'e'			{ return TType }
	/ 'd' 'e' 'r' 'i' 'v' 'i' 'n' 'g' !_:<isVar>
						{ return TDeriving }
	/ 'c' 'l' 'a' 's' 's' !_:<isVar>	{ return TClass }
	/ 'i' 'n' 's' 't' 'a' 'n' 'c' 'e' !_:<isVar>
						{ return TInstance }
	/ 'i' 'n' 'f' 'i' 'x' !_:<isVar>	{ return TInfix }
	/ 'i' 'n' 'f' 'i' 'x' 'l' !_:<isVar>	{ return TInfixl }
	/ 'i' 'n' 'f' 'i' 'x' 'r' !_:<isVar>	{ return TInfixr }
	/ 'i' 'm' 'p' 'o' 'r' 't' !_:<isVar>	{ return TImport }
	/ 'q' 'u' 'a' 'l' 'i' 'f' 'i' 'e' 'd' !_:<isVar>
						{ return TQualified }
	/ 'a' 's' !_:<isVar>			{ return TAs }
	/ 'h' 'i' 'd' 'i' 'n' 'g' !_:<isVar>	{ return THiding }
	/ 'm' 'o' 'd' 'u' 'l' 'e' !_:<isVar>	{ return TModule }
	/ '{' '-' '#'				{ return TOPragma }
	/ '#' '-' '}'				{ return TCPragma }
	/ '('					{ return TOParen }
	/ ')'					{ return TCParen }
	/ '{'					{ return TOBrace }
	/ '}'					{ return TCBrace }
	/ '['					{ return TOBracket }
	/ ']'					{ return TCBracket }
	/ ','					{ return TComma }
	/ '`'					{ return TBackquote }
	/ '~' !_:<isOp>				{ return TTilde }
	/ '@' !_:<isOp>				{ return TAt }
	/ '=' !_:<isOp>				{ return TEq }
	/ '\\' !_:<isOp>			{ return TBackslash }
	/ '|' !_:<isOp>				{ return TVBar }
	/ ':' ':' !_:<isOp>			{ return TTypeDef }
	/ '-' '>' !_:<isOp>			{ return TRightArrow }
	/ '<' '-' !_:<isOp>			{ return TLeftArrow }
	/ '=' '>' !_:<isOp>			{ return TDRightArrow }
	/ '.' '.' !_:<isOp>			{ return TDotDot }
	/ l:lit					{ return $ TLit l }
	/ qs:(q:con '.' { return q })* v:var	{ return $ TVar $ concatMap
							(++ ".") qs ++ v }
	/ qs:(q:con '.' { return q })* t:con	{ return $ TCon $ concatMap
							(++ ".") qs ++ t }
	/ ':' o:<isOp>*				{ return $ TOpCon $ ':' : o }
	/ !_:('-' '-' !_:<isOp>) o:<isOp>+	{ return $ TOp o }
	/ ';'					{ return $ TSemicolon }

var :: String = h:<isLower_> t:<isVar>*		{ return $ h : t }
con :: String = h:<isUpper> t:<isVar>*		{ return $ h : t }

lit :: Lit
	= '\'' c:<(`notElem` "'\\")> '\''	{ return $ CharL c }
	/ '\'' '\\' c:escChar '\''		{ return $ CharL c }
	/ '"' s:('\\' c:escChar { return c } / <(`notElem` "\\\"")>)* '"'
						{ return $ StringL s }
	/ r1:<isDigit>+ '.' r2:<isDigit>+	{ return $ RationalL $
							read (r1 ++ r2 ++ "%1") /
							fromIntegral (10 ^
								(length r2)) }
	/ i:<isDigit>+				{ return $ IntegerL $ read i }

escChar :: Char
	= '\\'					{ return '\\' }
	/ '\''					{ return '\'' }
	/ '"'					{ return '"' }
	/ 'n'					{ return '\n' }
	/ 't'					{ return '\t' }
	/ ds:<isDigit>+				{ return $ chr $ read ds }
	/ 'E' 'S' 'C'				{ return $ chr 27 }
	/ 'B' 'S'				{ return $ chr 8 }
	/ 'D' 'E' 'L'				{ return $ chr 127 }

space	= _:<(`elem` " \t")>
	/ _:('\n')[ gets $ null . fst ]
	/ '\n'+ _:<(`elem` " \t")>* &(!(_, x)):lxp[gets $ (x >) . head . fst ]
	/ '-' '-' _:<(/= '\n')>* '\n'
	/ _:comment

comment = '{' '-' !'#' _:comStr _:(_:comment _:comStr)* '-' '}'
comStr	= !_:('{' '-') !_:('-' '}') _ _:comStr
	/

|]
