module Main
where

--
-- Representation of proposed NineML XML syntax
-- for network descriptions.
--
-- Copyright 2014 Ivan Raikov and the Okinawa Institute of Science and
-- Technology.
--
-- Based on Haskell HXT Pickle test code from
-- http://www.haskell.org/haskellwiki/HXT/Conversion_of_Haskell_data_from/to_XML
-- ( hughperkins@gmail.com ) 

  
import Data.Map (Map, fromList, toList)
  
import Data.Maybe
import System.Exit
import Test.HUnit

import Text.XML.HXT.Core


type Ident	= String

data Expr
    = IntConst	Int
    | RealConst	Double
    | BoolConst Bool
    | Var       Ident
    | NExpr	NOp
    | UnExpr	UnOp  Expr
    | BinExpr	Op    Expr  Expr
      deriving (Eq, Show)

data Op
    = Add | Sub | Mul | Div
      deriving (Eq, Ord, Read, Show)

data UnOp
    = Neg | Log | Exp | Sqrt | Sin | Cos | Sinh | Cosh | Tanh 
      deriving (Eq, Ord, Read, Show)

data NOp
    = RandomUniform | RandomNormal
      deriving (Eq, Ord, Read, Show)

data ConnectionGenerator 
    = AllToAll 
    | OneToOne
    | AdjacencyFile String
    | AdjacencyPairs IntMap
    | ForEach Ident Ident
      deriving (Eq, Ord, Read, Show)
             

type PropertyMap = Map Ident Expr

data SetExpr = SetSingleton Ident
    | SetUnion SetExpr SetExpr
    | SetIntersection SetExpr SetExpr
    | SetDifference SetExpr SetExpr
      deriving (Eq, Show)

type IntMap   = Map Int Int


--  Projection ::= 
--    label           : String 
--    source          : Population set
--    target          : Population set
--    response        : Component -> PSR 
--    responsePort    : Symbol
--    plasticity      : Component -> Plasticity
--    rule            : ConnectionGenerator 
--    properties      : Property set
--    delay           : Component -> AlgebraicSystem 
--    delayPort       : Symbol 


data GroupDecl = Population  { label :: Ident, prototype :: Ident, number :: Integer }
               | Set Ident SetExpr
               | Projection  { label :: Ident, source :: Ident, target :: Ident,
                               response :: Ident, responsePort ::  Ident,
                               plasticity :: Ident, rule :: ConnectionGenerator,
                               pProperties :: PropertyMap,
                               delay :: Ident, delayPort :: Ident }
            deriving (Eq, Show)
               

data Decl = Properties PropertyMap
          | Component   { cLabel :: Ident, cDefinition :: Ident, cProperties :: PropertyMap, cInitialValues :: PropertyMap }
          | Group       [GroupDecl]
            deriving (Eq, Show)
               

type Model = [Decl]

-- ------------------------------------------------------------
--
-- the pickler definition for the data types

-- the main pickler

xpModel :: PU Model
xpModel = xpElem "model" $
	    xpAddFixedAttr "xmlns" "http://nineml.org/9ML/0.4" $
	    xpickle

xpMissingRootElement	:: PU Model
xpMissingRootElement 	= xpickle


instance XmlPickler Double where
    xpickle = xpPrim

instance XmlPickler UnOp where
    xpickle = xpPrim

instance XmlPickler Op where
    xpickle = xpPrim

instance XmlPickler NOp where
    xpickle = xpPrim


instance XmlPickler Expr where
    xpickle = xpAlt tag ps
	where
	tag (IntConst _    ) = 0
	tag (RealConst _   ) = 1
	tag (BoolConst _   ) = 2
	tag (Var _         ) = 3
	tag (NExpr  _      ) = 4
	tag (UnExpr _ _    ) = 5
	tag (BinExpr _ _ _ ) = 6
	ps = [ xpWrap ( IntConst
		      , \ (IntConst i ) -> i
                      ) $
               ( xpElem "int"   $
		 xpAttr "value" $
		 xpickle
               )

	     , xpWrap ( RealConst
		      , \ (RealConst i ) -> i
                      ) $
               ( xpElem "real"   $
		 xpAttr "value" $
		 xpickle
               )
	     , xpWrap ( BoolConst
		      , \ (BoolConst b) -> b
                      ) $
               ( xpElem "bool"  $
		 xpAttr "value" $
		 xpWrap (toEnum, fromEnum) xpickle
               )

	     , xpWrap ( Var
		      , \ (Var n)       -> n
                      ) $
               ( xpElem "var"   $
		 xpAttr "name"  $
		 xpText
               )

	     , xpWrap ( NExpr
		      , \ (NExpr op) -> op
                      ) $
               ( xpElem "nexpr" $
		 (xpAttr "op" xpickle)
               )
	     , xpWrap ( uncurry UnExpr
		      , \ (UnExpr op e) -> (op, e)
                      ) $
               ( xpElem "unexpr" $
		 xpPair (xpAttr "op" xpickle)
                         xpickle
               )

	     , xpWrap ( uncurry3 $ BinExpr
		      , \ (BinExpr op e1 e2) -> (op, e1, e2)
                      ) $
               ( xpElem "binexpr" $
		 xpTriple (xpAttr "op" xpickle)
                           xpickle
                           xpickle
               )
	     ]

xpProperties :: PU PropertyMap
xpProperties = xpWrap ( fromList, toList ) $
               xpList $ xpElem "Property" $
                      xpPair (xpAttr "name" xpText) 
                             (xpElem "value" $ xpickle)

xpInitialValues :: PU PropertyMap
xpInitialValues = xpWrap ( fromList, toList ) $
                  xpList $ xpElem "Initial" $
                         xpPair (xpAttr "name" xpText) 
                                    (xpElem "value" $ xpickle)


instance XmlPickler SetExpr where
    xpickle = xpAlt tag ps
	where
	tag (SetSingleton _      ) = 0
	tag (SetUnion _ _        ) = 1
	tag (SetIntersection _ _ ) = 2
	tag (SetDifference   _ _ ) = 3
        ps = [ 
               xpWrap ( SetSingleton
		      , \ (SetSingleton n) -> n
                      ) $
               ( xpElem "singleton" $
		 xpAttr "name" $
		 xpText
               ),
               xpWrap ( uncurry SetUnion
		      , \ (SetUnion x y ) -> (x,y)
                      ) $
               ( xpElem "union" $
		 xpPair xpickle xpickle
               ),
               xpWrap ( uncurry SetIntersection
		      , \ (SetIntersection x y ) -> (x,y)
                      ) $
               ( xpElem "intersection" $
		 xpPair xpickle xpickle
               ),
               xpWrap ( uncurry SetDifference
		      , \ (SetDifference x y ) -> (x,y)
                      ) $
               ( xpElem "difference" $
		 xpPair xpickle xpickle
               )
             ]

instance XmlPickler ConnectionGenerator where
    xpickle = xpAlt tag ps
	where
	tag (AllToAll         ) = 0
	tag (OneToOne         ) = 1
	tag (AdjacencyFile  _ ) = 2
	tag (AdjacencyPairs _ ) = 3
	tag (ForEach _ _      ) = 4
        ps = [ 
               xpWrap ( \ () -> AllToAll, \ AllToAll -> () ) $ xpElem "AllToAll" xpUnit,
               xpWrap ( \ () -> OneToOne, \ OneToOne -> () ) $ xpElem "OneToOne" xpUnit,
               xpWrap ( AdjacencyFile
	              , \ (AdjacencyFile n) -> n
                      ) $
               xpElem "AdjacencyFile" $
	              xpAttr "name" $ xpText ,
               xpWrap ( AdjacencyPairs
	              , \ (AdjacencyPairs xs ) -> xs
                         ) $
               xpElem "AdjacencyPairs" $
                      xpWrap ( fromList, toList ) $
                             xpList $ xpElem "AdjacencyPair" $ xpPair (xpAttr "s" xpInt) (xpAttr "t" xpInt),
               xpWrap ( uncurry ForEach
	              , \ (ForEach comp port ) -> (comp, port)
                      ) $
               xpElem "ForEach" $
                      xpPair (xpElem "reference" xpText)
                             (xpElem "connectionPort" xpText) 
             ]


instance XmlPickler Decl where
    xpickle = xpAlt tag ps
	where
	tag ( Properties _      ) = 0
	tag ( Component _ _ _ _ ) = 1
	tag ( Group _           ) = 2
	ps = [ xpElem "properties" $ xpWrap ( Properties,
		                              \ (Properties p) -> p
                                            ) $ xpProperties,
	       xpElem "component" $
                      xpWrap (  uncurry4 Component,
		                \ t -> (cLabel t, cDefinition t, cProperties t, cInitialValues t)
                             ) $
                      xp4Tuple (xpAttr "name" xpText)
                               (xpElem "definition" xpText)
                               (xpElem "properties" xpProperties)
                               (xpElem "initialValues" xpInitialValues),
	       xpElem "group" $
                      xpWrap (  Group,
		                \ (Group decls) -> decls
                             ) $
                      xpList xpickle
	     ]

instance XmlPickler GroupDecl where
    xpickle = xpAlt tag ps
	where
	tag ( Population _ _ _ ) = 0
	tag ( Set _ _          ) = 1
	tag ( Projection _ _ _ _ _ _ _ _ _ _) = 2
	ps = [ 
	       xpElem "population" $
                      xpWrap (  uncurry3 Population,
		                \ t -> (label t, prototype t, number t)
                             ) $
                      xpTriple (xpAttr "name" xpText)
                                   (xpElem "prototype" xpText)
                                   (xpElem "number" xpickle),
	       xpElem "set" $
                      xpWrap (  uncurry Set,
		                \ (Set i t) -> (i,t)
                             ) $ (xpPair (xpAttr "name" xpText) xpickle),
	       xpElem "projection" $
                      xpWrap ( \ ((n,s,t,r,rp,pl,rule,props,d,dp)) -> Projection n s t r rp pl rule props d dp,
		               \ t ->
                                   ( label t, source t, target t,
                                     response t, responsePort t,
                                     plasticity t, rule t, pProperties t,
                                     delay t, delayPort t )
                             ) $
                      xp10Tuple (xpAttr "name" xpText)
                                (xpElem "source" xpText)
                                (xpElem "target" xpText)
                                (xpElem "response" xpText)
                                (xpElem "responsePort" xpText)
                                (xpElem "plasticity" xpText)
                                (xpElem "rule" xpickle)
                                (xpElem "properties" $ xpProperties)
                                (xpElem "delay" xpText)
                                (xpElem "delayPort" xpText)
	     ]

-- ------------------------------------------------------------
--
-- example models

models	:: [Model]
models	= [p0, p1]

p0 = []

p1 = [Properties
      (fromList
       [
        -- Scales the size of the network (total 5*order neurons)
        ("order", IntConst 2500),
        -- Number of excitatory neurons
        ("Ne", BinExpr Mul (IntConst 4) (Var "order")),
        -- Number of inhibitory neurons 
        ("Ni", BinExpr Mul (IntConst 1) (Var "order")),
        -- Connectivity probability 
        ("epsilon", RealConst 0.1),
        -- Total number of excitatory synapses 
        ("Ce", BinExpr Mul (Var "epsilon") (Var "Ne")),
        -- Total number of inhibitory synapses 
        ("Ci", BinExpr Mul (Var "epsilon") (Var "Ni")),
        -- Total number of external synapses 
        ("Cext", (Var "Ce")),
        -- Global delay for all neurons in the group
        ("delay", (RealConst 1.5)),
        -- Synaptic weight
        ("J", (RealConst 0.1)),
        -- Relative strength of inhibitory synapses
        ("g", (RealConst 5.0)),
        -- nu_ext / nu_thr
        ("eta", (RealConst 2.0)),
        -- Excitatory weights
        ("Je", (Var "J")),
        -- Inhibitory weights
        ("Ji", (BinExpr Mul (UnExpr Neg (Var "g")) (Var "Je"))),
        -- External weights
        ("Jext", (Var "Je")),
        -- Firing threshold
        ("theta", (RealConst 20.0)),
        -- Membrane time constant
        ("tau", (RealConst 20.0)),
        -- Synapse time constant
        ("tau_syn", (RealConst 0.5)),
        -- Threshold rate = theta / (Je * Ce * tau * exp (1.0) * tau_syn
        ("nu_thresh", (BinExpr 
                       Div
                       (Var "theta")
                       (BinExpr Mul (Var "Je")
                                    (BinExpr Mul (Var "Ce")
                                                 (BinExpr Mul (Var "tau")
                                                          (BinExpr Mul (UnExpr Exp (RealConst 1.0))
                                                                   (Var "tau_syn"))
                                                          ))
                       ))
         ),
        -- External rate per synapse
        ("nu_ext", BinExpr Mul (Var "eta") (Var "nu_thresh")),
        -- Mean input spiking rate
        ("input_rate", BinExpr Mul (RealConst 1000.0) (Var "nu_ext"))
       ]),
      -- Excitatory neuron
      Component "E" "BrunelIaF.xml"
                (fromList
                 [
                  ("tau", (Var "tau")),
                  ("theta", (Var "theta")),
                  ("tau_rp", (RealConst 2.0)),
                  ("Vreset", (RealConst 10.0)),
                  ("R", (RealConst 1.5))
                 ])
                (fromList
                 [
                  ("Isyn", (RealConst 1.5)),
                  ("V", (NExpr RandomUniform)),
                  ("t_rpend", (RealConst 0.0)),
                  ("spikeOutput", (BoolConst False)),
                  ("refractoryEnd", (BoolConst False))
                 ]),
      -- Inhibitory neuron
      Component "I" "BrunelIaF.xml"
                (fromList
                 [
                  ("tau", (Var "tau")),
                  ("theta", (Var "theta")),
                  ("tau_rp", (RealConst 2.0)),
                  ("Vreset", (RealConst 10.0)),
                  ("R", (RealConst 1.5))
                 ])
                (fromList
                 [
                  ("Isyn", (RealConst 0.0)),
                  ("V", (NExpr RandomUniform)),
                  ("t_rpend", (RealConst 0.0)),
                  ("spikeOutput", (BoolConst False)),
                  ("refractoryEnd", (BoolConst False))
                 ]),
      -- External stimulus
      Component "Ext" "http://www.NineML.org/stim/Poisson.9ml"
                (fromList
                 [
                  ("rate", (Var "input_rate"))
                 ])
                (fromList []),
      -- Synaptic dynamics
      Component "Syn" "AlphaPSR.xml"
                (fromList
                 [
                  ("tau_syn", (Var "tau_syn"))
                 ])
                (fromList 
                 [
                  ("Ispike", (RealConst 0.0)),
                  ("Isyn", (RealConst 0.0))
                 ]),
      -- Constant synaptic weight for all types of connections
      Component "ExternalPlasticity" "ConstantPlasticity.xml"
                (fromList [])
                (fromList 
                 [
                  ("weight", (Var "Jext"))
                 ]),
      Component "ExcitatoryPlasticity" "ConstantPlasticity.xml"
                (fromList [])
                (fromList 
                 [
                  ("weight", (Var "Je"))
                 ]),
      Component "InhibitoryPlasticity" "ConstantPlasticity.xml"
                (fromList [])
                (fromList 
                 [
                  ("weight", (Var "Ji"))
                 ]),
      -- Random uniform connection probability
      Component "RandomUniformConnection" "RandomUniformConnection.xml"
                (fromList 
                 [
                  ("epsilon", (Var "epsilon"))
                 ])
                (fromList []),
      Group [
                 Population  { label="Exc", prototype="E", number=10000 },
                 Population  { label="Inh", prototype="I", number=2500 },
                 Population  { label="Ext", prototype="Ext", number=1250 },
                 Set "All neurons" (SetUnion (SetSingleton "Exc") (SetSingleton "Inh")),
                 Projection { label="External", source="Ext", target="All neurons",
                              response="Syn", responsePort="Isyn",
                              plasticity="ExternalPlasticity", rule=AllToAll,
                              pProperties=(fromList []),
                              delay="GlobalDelay", delayPort="d"
                              },
                 Projection { label="Excitatory", source="Exc", target="All neurons",
                              response="Syn", responsePort="Isyn",
                              plasticity="ExcitatoryPlasticity", 
                              rule=ForEach "RandomUniformConnection" "connection",
                              pProperties=(fromList []),
                              delay="GlobalDelay", delayPort="d"
                              },
                 Projection { label="Inhibitory", source="Inh", target="All neurons",
                              response="Syn", responsePort="Isyn",
                              plasticity="InhibitoryPlasticity", 
                              rule=ForEach "RandomUniformConnection" "connection",
                              pProperties=(fromList []),
                              delay="GlobalDelay", delayPort="d"
                              }
            ] 
     ]

-- ------------------------------------------------------------

-- |
-- the complete set of test cases

pickleUnpickleTests	:: Test
pickleUnpickleTests
    = TestLabel "pickle/unpickle tests with example programs" $
      TestList $
      map mkTests models
    where
    mkTests p
	= TestList $
	  [ TestCase $
	    assertEqual "pickleDoc/unpickleDoc without XML serialisation: " [p] res1

	  , TestCase $
	    assertEqual "pickleDoc/unpickleDoc with xshow/xread: " [p] res2

	  , TestCase $
	    do
	    res <- res4
	    assertEqual "pickle/unpickle with readFromString: " [p] res

	  , TestCase $
	    res5 >>= 
	    assertEqual "pickle/unpickle with writeDocument/readDocument: " [p]

	  , TestCase $
	    res6 >>= 
	    assertEqual "pickle/unpickle with xpickleDocument/xunpickleDocument: " [p]

	  , TestCase $
	    res7 >>= 
	    assertEqual "pickle/unpickle with DTD validation xpickleDocument/xunpickleDocument: " [p]
	  ]
	where
	res1	:: [Model]
	res1 = maybeToList . unpickleDoc xpModel . pickleDoc xpModel $ p

	res2	:: [Model]
	res2 = runLA
	       ( xshow ( arr (pickleDoc xpModel)
			 >>>
			 getChildren
		       )
		 >>>
		 root [] [xread]
		 >>>
		 arrL (maybeToList . unpickleDoc xpModel)
	       ) p

	res4	:: IO [Model]
	res4 = runX
	       ( constA p
		 >>>
		 arr (pickleDoc xpModel)			-- Model => XmlTree
		 >>>
		 writeDocumentToString []			-- XmlTree => String
		 >>>
		 readFromString [ withValidate no ]		-- String => XmlTree
		 >>>
		 arrL (maybeToList . unpickleDoc xpModel)	-- XmlTree => Model
	       )

	res5	:: IO [Model]					-- the most important case
								-- for persistent data storage
								-- and message passing
	res5 = runX
	       ( constA p					-- take the Model value
		 >>>
		 arr (pickleDoc xpModel)			-- Model => XmlTree
		 >>>
		 writeDocument [ withIndent yes			-- XmlTree => formated external XML document
			       ] "pickle.xml"
		 >>>
		 readDocument  [ withRemoveWS yes		-- formated external XML document => XmlTree
			       , withValidate no
			       ] "pickle.xml"
		 >>>
		 arrL (maybeToList . unpickleDoc xpModel)	-- XmlTree => Model
	       )

	res6	:: IO [Model]					-- the most important case
								-- for persistent data storage
								-- and message passing
								-- same as res5, but the convenient way
	res6 = runX
	       ( constA p					-- take the Model value
		 >>>
		 xpickleDocument   xpModel
                                 [ withIndent yes		-- Model => formated external XML document
				 ] "model.xml"
		 >>>
		 xunpickleDocument xpModel
                                   [ withRemoveWS yes		-- formated external XML document => Model
				   , withValidate no
				   ] "model.xml"
	       )
	res7	:: IO [Model]					-- the most important case
								-- for persistent data storage
								-- and message passing
								-- same as res5, but the convenient way
	res7 = runX
	       ( constA p					-- take the Model value
		 >>>
		 xpickleDocument   xpModel
                                 [ withIndent yes		-- Model => formated external XML document
				 , withSysAttr a_addDTD v_1	-- with inline DTD
				 ] "model.xml"
		 >>>
		 xunpickleDocument xpModel
                                   [ withRemoveWS yes		-- formated external XML document => Model
				   , withValidate yes
				   ] "model.xml"
	       )

allTests	:: Test
allTests
    = TestList
      [ pickleUnpickleTests
      -- , pickleXshowTests
      ]

main	:: IO ()
main
    = do
      c <- runTestTT allTests
      putStrLn $ show c
      let errs = errors c
	  fails = failures c
      exitWith (codeGet errs fails)

codeGet	:: Int -> Int -> ExitCode
codeGet errs fails
    | fails > 0       = ExitFailure 2
    | errs > 0        = ExitFailure 1
    | otherwise       = ExitSuccess

-- ----------------------------------------------------------
