{-# LANGUAGE QuasiQuotes, TypeFamilies, PackageImports #-}

module Language.Haskell.PapParser (
	haskell
) where

import Prelude hiding (exp)
import Text.Papillon
import Language.Haskell.TH hiding (match)
import Language.Haskell.TH.Quote
import "monads-tf" Control.Monad.State
import "monads-tf" Control.Monad.Error

import Data.Char
import Data.Maybe
import Control.Applicative
import Control.Arrow

type ParseM = State (Int, ([Int], [Tkn]))

pushX :: Int -> ParseM Bool
pushX x = modify (second $ first (x :)) >> return True

popX :: ParseM Bool
popX = modify (second $ first tail) >> return True

setQueue :: [Tkn] -> ParseM ()
setQueue tkns = modify (second $ second $ const tkns)

popQueue :: ParseM Tkn
popQueue = do
	(x, (xs, tkn : tkns)) <- get
	put (x, (xs, tkns))
	return tkn

getSemiVCBs :: Int -> [Int] -> [Tkn]
getSemiVCBs _ [] = []
getSemiVCBs x0 (x1 : xs)
	| x0 > x1 = []
	| x0 == x1 = [TSemicolon]
	| otherwise = TVCBrace : getSemiVCBs x0 xs

haskell :: QuasiQuoter
haskell = QuasiQuoter {
	quoteExp = return . haskellExp,
	quotePat = return . haskellPat,
	quoteType = return . haskellTyp,
	quoteDec = undefined
 }

haskellExp :: String -> Exp
haskellExp src = case flip evalState (0, ([], [])) $ runErrorT $ expA $ parse src of
	Right (p, _) -> p
	Left _ -> error "parse error"

haskellPat :: String -> Pat
haskellPat src = case flip evalState (0, ([], [])) $ runErrorT $ patA $ parse src of
	Right (p, _) -> p
	Left _ -> error "parse error"

haskellTyp :: String -> Type
haskellTyp src = case flip evalState (0, ([], [])) $ runErrorT $ typA $ parse src of
	Right (p, _) -> p
	Left _ -> error "parse error"

testTkn :: String -> Tkn
testTkn src = case flip evalState (0, ([], [])) $ runErrorT $ tkn $ parse src of
	Right (p, _) -> p
	Left _ -> error "parse error"

testLit :: String -> Lit
testLit src = case flip evalState (0, ([], [])) $ runErrorT $ lit $ parse src of
	Right (p, _) -> p
	Left _ -> error "parse error"

isLower_ :: Char -> Bool
isLower_ = (||) <$> isLower <*> (`elem` "_")

isVar :: Char -> Bool
isVar = (||) <$> isAlphaNum <*> (`elem` "'_")

isOp :: Char -> Bool
isOp = (`elem` "!#$%&*+./<=>?@\\^|-~")

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
	| TTypeDef
	| TRightArrow
	| TLeftArrow
	| TForall
	| TBackslash
	| TIf
	| TThen
	| TElse
	| TLet
	| TIn
	| TCase
	| TOf
	| TDo
	| TSemicolon
	deriving Show

[papillon|

monad: ParseM

expA :: Exp = e:exp _:space* !_			{ return e }

exp :: Exp = e:expInfix				{ return e }

expInfix :: Exp
	= lft:expApp ors:(o:operator r:expApp { return (o, r) })*
						{ return $ foldl
							(\l (o, r) -> UInfixE l o r)
							lft ors }

operator :: Exp
	= (TOp o):lx				{ return $ VarE $ mkName o }

expApp :: Exp
	= f:exp1 as:exp1*			{ return $ foldl AppE f as }

exp1 :: Exp
	= (TVar v):lx				{ return $ VarE $ mkName v }
	/ (TCon c):lx				{ return $ ConE $ mkName c }
	/ (TLit l):lx				{ return $ LitE l }
	/ !_:(TOParen:lx !_:expApp TCParen:lx)
		TOParen:lx l:exp? o:operator r:exp? TCParen:lx
						{ return $ InfixE l o r }
	/ TOParen:lx TCParen:lx			{ return $ TupE [] }
	/ TOParen:lx e:exp TCParen:lx		{ return e }
	/ TOParen:lx e0:exp es:(TComma:lx e:exp { return e })+ TCParen:lx
						{ return $ TupE $ e0 : es }
	/ TBackslash:lx ps:pat1+ TRightArrow:lx e:exp
						{ return $ LamE ps e }
	/ TIf:lx p:exp TThen:lx t:exp TElse:lx e:exp
						{ return $ CondE p t e }
	/ TLet:lx TOBrace:lx ds:decs TCBrace:lx TIn:lx e:exp
						{ return $ LetE ds e }
	/ TLet:lx &(_, x):lxp[pushX x] ds:decs _:(TVCBrace:lx)? TIn:lx[popX] e:exp
						{ return $ LetE ds e }
--	/ TCase:lx e:exp TOf:lx ms:matches	{ return $ CaseE e ms }
	/ TDo:lx TOBrace:lx ss:stmts TCBrace:lx	{ return $ DoE ss }
	/ TDo:lx &(_, x):lxp[pushX x] ss:stmts TVCBrace:lx[popX]
						{ return $ DoE ss }

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

typA :: Type = p:typ _:space* !_		{ return p }

typ :: Type = t:typApp				{ return t }

typApp :: Type
	= t:typ1 ts:typ1*			{ return $ foldl AppT t ts }

typ1 :: Type
	= TForall:lx tvs:tyVarBndr+ (TOp "."):lx t:typ
						{ return $ ForallT tvs [] t }
	/ (TVar v):lx				{ return $ VarT $ mkName v }
	/ (TCon c):lx				{ return $ ConT $ mkName c }
	/ TOParen:lx TCParen:lx			{ return $ TupleT 0 }
	/ TOParen:lx ts:typTup TCParen:lx	{ return $ foldl AppT
							(TupleT $ length ts) ts }
	/ TOParen:lx TRightArrow:lx TCParen:lx	{ return ArrowT }
	/ TOBracket:lx t:typ TCBracket:lx	{ return $ AppT ListT t }

typTup :: [Type] = t0:typ ts:(TComma:lx t:typ { return t })*
						{ return $ t0 : ts }

decs :: [Dec]
	= md:dec? mds:(TSemicolon:lx md:dec? { return md })*
						{ return $ maybeToList md ++
							catMaybes mds }

dec :: Dec
	= p:pat TEq:lx e:exp			{ return $ ValD p (NormalB e) [] }

matches :: [Match]
	= mm:match? mms:(TSemicolon:lx mm:match? { return mm })*
						{ return $ maybeToList mm ++
							catMaybes mms }

match :: Match
	= p:pat TRightArrow:lx e:exp		{ return $ Match p (NormalB e) [] }

stmts :: [Stmt]
	= ms:stmt? mss:(TSemicolon:lx ms:stmt? { return ms })*
						{ return $ maybeToList ms ++
							catMaybes mss }

stmt :: Stmt
	= e:exp					{ return $ NoBindS e }

fieldPats :: [FieldPat]
	= fp:fieldPt fps:(TComma:lx fp:fieldPt { return fp })*
						{ return $ fp : fps }

fieldPt :: FieldPat
	= (TVar v):lx TEq:lx p:pat		{ return (mkName v, p) }

tyVarBndr :: TyVarBndr
	= (TVar v):lx				{ return $ PlainTV $ mkName v }
--	/ (TVar v):lx TTypeDef:lx t:typ		{ return $ KindedTV (mkName v) t }

lx :: Tkn
	= _:position[not . null . snd . snd <$> get]
						{ popQueue }
	/ (!(t, _)):lxp				{ return t }

lxp :: (Tkn, Int)
	= _:space* (!ListPos (CharPos (_, x))):position t:tkn
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
	/ '('					{ return TOParen }
	/ ')'					{ return TCParen }
	/ '{'					{ return TOBrace }
	/ '}'					{ return TCBrace }
	/ '['					{ return TOBracket }
	/ ']'					{ return TCBracket }
	/ ','					{ return TComma }
	/ '~' !_:<isOp>				{ return TTilde }
	/ '@' !_:<isOp>				{ return TAt }
	/ '=' !_:<isOp>				{ return TEq }
	/ '\\' !_:<isOp>			{ return TBackslash }
	/ ':' ':' !_:<isOp>			{ return TTypeDef }
	/ '-' '>' !_:<isOp>			{ return TRightArrow }
	/ '<' '-' !_:<isOp>			{ return TLeftArrow }
	/ l:lit					{ return $ TLit l }
	/ qs:(q:con '.' { return q })* v:var	{ return $ TVar $ concatMap
							(++ ".") qs ++ v }
	/ qs:(q:con '.' { return q })* t:con	{ return $ TCon $ concatMap
							(++ ".") qs ++ t }
	/ ':' o:<isOp>+				{ return $ TOpCon $ ':' : o }
	/ o:<isOp>+				{ return $ TOp o }
	/ _:semicolon				{ return $ TSemicolon }
	/ _:vcbrace				{ return $ TVCBrace }

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

space	= _:<(`elem` " \t")>
	/ _:('\n')[ null . fst . snd <$> get ]
	/ '\n'+ _:<(`elem` " \t")>* &(_, x):lxp[
		(x >) . head . fst . snd <$> get ]

semicolon
	= ';'
	/ '\n'+ _:<(`elem` " \t")>* &(_, x):lxp[
		(x ==) . head . fst . snd <$> get ]

vcbrace	= '\n'+ _:<(`elem` " \t")>* &(_, x):lxp[
		modify (first $ const x) >>
		(x <) . head . fst . snd <$> get ]
		{ gets fst >>= (\x ->
			(getSemiVCBs x . fst . snd <$> get) >>=
			modify . (second . second . const)) }
	/ !_:[ not . null . fst . snd <$> get ]
		{ gets (fst . snd) >>= (\x ->
			return (replicate (length x) TVCBrace) >>=
			modify . (second . second . const)) }
-- getSemiVCBs :: Int -> [Int] -> [Tkn]

|]
