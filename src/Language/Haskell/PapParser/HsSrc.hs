module Language.Haskell.PapParser.HsSrc (
	HsSrc(..),
	Pragma(..),
	Module(..),
	Import(..),
	Export(..),
	Member(..)
) where

import Language.Haskell.TH hiding (Pragma)
import Language.Haskell.TH.PprLib (
	text, sep, comma, parens, punctuate, (<>), (<+>), vcat, empty, ($$),
	doubleQuotes)
import Language.Haskell.PapParser.Ppr (ppr')

data HsSrc = HsSrc {
	srcPragmas :: [Pragma],
	srcModule :: Module,
	srcImports :: [Import],
	srcDecs :: [Dec]
 } deriving Show

instance Ppr HsSrc where
	ppr (HsSrc ps ms is ds) = ppr ps $$ ppr ms $$ ppr is $$ ppr' ds

data Pragma
	= LanguagePragma [String]
	| OtherPragma String
	deriving Show

instance Ppr Pragma where
	ppr (LanguagePragma ps) = text "{-#" <+> text "LANGUAGE" <+>
		sep (punctuate comma (map text ps)) <+>
		text "#-}"
	ppr (OtherPragma p) = text "{-#" <+> text p <+> text "#-}"

data Module = Module {
	modName :: String,
	exportList :: Maybe [Export]
 } deriving Show

instance Ppr Module where
 	ppr (Module n Nothing) = text "module" <+> text n <+> text "where"
	ppr (Module n (Just el)) =
		text "module" <+> text n <+> parens (ppr el) <+> text "where"

data Import = Import {
	packageName :: Maybe String,
	isQualified :: Bool,
	impModName ::  String,
	asName :: Maybe String,
	isHiding :: Bool,
	importList :: Maybe [Export]
 } deriving Show

instance Ppr Import where
	ppr (Import pn iq n an ih il) =
		text "import" <+>
		maybe empty (doubleQuotes . text) pn <+>
		(if iq then text "qualified" else empty) <+>
		text n <+>
		maybe empty ((text "as" <+>) . text) an <+>
		(if ih then text "hiding" else empty) <+>
		maybe empty (parens . ppr) il
	ppr_list is = vcat $ map ppr is

data Export
	= EMem Member
	| EType String (Maybe [Member])
	deriving Show

instance Ppr Export where
	ppr (EMem m) = ppr m
	ppr (EType n Nothing) = text n
	ppr (EType n (Just ms)) = text n <> parens (ppr ms)
	ppr_list ms = sep $ punctuate comma $ map ppr ms

data Member
	= MVar String
	| MOp String
	deriving Show

instance Ppr Member where
	ppr (MVar n) = text n
	ppr (MOp n) = parens $ text n
	ppr_list ms = sep $ punctuate comma $ map ppr ms
