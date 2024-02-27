{-# OPTIONS_GHC -w #-}
module Parser where

import Lexer
import Process
import BasicType
import Linearity.Use
import Linearity.Combination as C
import Linearity.Type
import Linearity.Process

import qualified Data.Map as M

import Data.Bool.Unicode
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.5

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7
	| HappyAbsSyn8 t8
	| HappyAbsSyn9 t9
	| HappyAbsSyn10 t10
	| HappyAbsSyn11 t11
	| HappyAbsSyn12 t12
	| HappyAbsSyn13 t13
	| HappyAbsSyn14 t14
	| HappyAbsSyn15 t15
	| HappyAbsSyn16 t16
	| HappyAbsSyn17 t17
	| HappyAbsSyn18 t18
	| HappyAbsSyn19 t19
	| HappyAbsSyn20 t20

action_0 (21) = happyShift action_4
action_0 (22) = happyShift action_5
action_0 (23) = happyShift action_6
action_0 (24) = happyShift action_7
action_0 (25) = happyShift action_8
action_0 (26) = happyShift action_9
action_0 (27) = happyShift action_10
action_0 (29) = happyShift action_11
action_0 (30) = happyShift action_12
action_0 (31) = happyShift action_13
action_0 (33) = happyShift action_14
action_0 (36) = happyShift action_15
action_0 (41) = happyShift action_16
action_0 (50) = happyShift action_17
action_0 (55) = happyShift action_18
action_0 (62) = happyShift action_19
action_0 (70) = happyShift action_20
action_0 (4) = happyGoto action_21
action_0 (5) = happyGoto action_2
action_0 (15) = happyGoto action_3
action_0 _ = happyFail

action_1 (21) = happyShift action_4
action_1 (22) = happyShift action_5
action_1 (23) = happyShift action_6
action_1 (24) = happyShift action_7
action_1 (25) = happyShift action_8
action_1 (26) = happyShift action_9
action_1 (27) = happyShift action_10
action_1 (29) = happyShift action_11
action_1 (30) = happyShift action_12
action_1 (31) = happyShift action_13
action_1 (33) = happyShift action_14
action_1 (36) = happyShift action_15
action_1 (41) = happyShift action_16
action_1 (50) = happyShift action_17
action_1 (55) = happyShift action_18
action_1 (62) = happyShift action_19
action_1 (70) = happyShift action_20
action_1 (5) = happyGoto action_2
action_1 (15) = happyGoto action_3
action_1 _ = happyFail

action_2 (40) = happyShift action_62
action_2 _ = happyReduce_1

action_3 (39) = happyShift action_43
action_3 (43) = happyShift action_44
action_3 (44) = happyShift action_45
action_3 (45) = happyShift action_46
action_3 (46) = happyShift action_47
action_3 (47) = happyShift action_48
action_3 (51) = happyShift action_49
action_3 (52) = happyShift action_50
action_3 (53) = happyShift action_51
action_3 (54) = happyShift action_52
action_3 (56) = happyShift action_53
action_3 (64) = happyShift action_54
action_3 (65) = happyShift action_55
action_3 (66) = happyShift action_56
action_3 (70) = happyShift action_57
action_3 (71) = happyShift action_58
action_3 (72) = happyShift action_59
action_3 (73) = happyShift action_60
action_3 (75) = happyShift action_61
action_3 _ = happyFail

action_4 (25) = happyShift action_42
action_4 _ = happyFail

action_5 _ = happyReduce_34

action_6 _ = happyReduce_35

action_7 _ = happyReduce_36

action_8 _ = happyReduce_37

action_9 (22) = happyShift action_5
action_9 (23) = happyShift action_6
action_9 (24) = happyShift action_7
action_9 (25) = happyShift action_8
action_9 (26) = happyShift action_9
action_9 (29) = happyShift action_11
action_9 (30) = happyShift action_12
action_9 (41) = happyShift action_16
action_9 (50) = happyShift action_17
action_9 (55) = happyShift action_18
action_9 (15) = happyGoto action_41
action_9 _ = happyFail

action_10 (25) = happyShift action_38
action_10 (41) = happyShift action_39
action_10 (74) = happyShift action_40
action_10 (14) = happyGoto action_37
action_10 _ = happyFail

action_11 (22) = happyShift action_5
action_11 (23) = happyShift action_6
action_11 (24) = happyShift action_7
action_11 (25) = happyShift action_8
action_11 (26) = happyShift action_9
action_11 (29) = happyShift action_11
action_11 (30) = happyShift action_12
action_11 (41) = happyShift action_16
action_11 (50) = happyShift action_17
action_11 (55) = happyShift action_18
action_11 (15) = happyGoto action_36
action_11 _ = happyFail

action_12 (22) = happyShift action_5
action_12 (23) = happyShift action_6
action_12 (24) = happyShift action_7
action_12 (25) = happyShift action_8
action_12 (26) = happyShift action_9
action_12 (29) = happyShift action_11
action_12 (30) = happyShift action_12
action_12 (41) = happyShift action_16
action_12 (50) = happyShift action_17
action_12 (55) = happyShift action_18
action_12 (15) = happyGoto action_35
action_12 _ = happyFail

action_13 (22) = happyShift action_5
action_13 (23) = happyShift action_6
action_13 (24) = happyShift action_7
action_13 (25) = happyShift action_8
action_13 (26) = happyShift action_9
action_13 (29) = happyShift action_11
action_13 (30) = happyShift action_12
action_13 (41) = happyShift action_16
action_13 (50) = happyShift action_17
action_13 (55) = happyShift action_18
action_13 (15) = happyGoto action_34
action_13 _ = happyFail

action_14 (22) = happyShift action_5
action_14 (23) = happyShift action_6
action_14 (24) = happyShift action_7
action_14 (25) = happyShift action_8
action_14 (26) = happyShift action_9
action_14 (29) = happyShift action_11
action_14 (30) = happyShift action_12
action_14 (41) = happyShift action_16
action_14 (50) = happyShift action_17
action_14 (55) = happyShift action_18
action_14 (15) = happyGoto action_33
action_14 _ = happyFail

action_15 (25) = happyShift action_32
action_15 (6) = happyGoto action_30
action_15 (7) = happyGoto action_31
action_15 _ = happyFail

action_16 (22) = happyShift action_5
action_16 (23) = happyShift action_6
action_16 (24) = happyShift action_7
action_16 (25) = happyShift action_28
action_16 (26) = happyShift action_9
action_16 (29) = happyShift action_11
action_16 (30) = happyShift action_12
action_16 (41) = happyShift action_16
action_16 (42) = happyShift action_29
action_16 (50) = happyShift action_17
action_16 (55) = happyShift action_18
action_16 (15) = happyGoto action_27
action_16 _ = happyFail

action_17 (22) = happyShift action_5
action_17 (23) = happyShift action_6
action_17 (24) = happyShift action_7
action_17 (25) = happyShift action_8
action_17 (26) = happyShift action_9
action_17 (29) = happyShift action_11
action_17 (30) = happyShift action_12
action_17 (41) = happyShift action_16
action_17 (50) = happyShift action_17
action_17 (55) = happyShift action_18
action_17 (15) = happyGoto action_26
action_17 _ = happyFail

action_18 (22) = happyShift action_5
action_18 (23) = happyShift action_6
action_18 (24) = happyShift action_7
action_18 (25) = happyShift action_8
action_18 (26) = happyShift action_9
action_18 (29) = happyShift action_11
action_18 (30) = happyShift action_12
action_18 (41) = happyShift action_16
action_18 (50) = happyShift action_17
action_18 (55) = happyShift action_18
action_18 (15) = happyGoto action_25
action_18 _ = happyFail

action_19 (21) = happyShift action_4
action_19 (22) = happyShift action_5
action_19 (23) = happyShift action_6
action_19 (24) = happyShift action_7
action_19 (25) = happyShift action_8
action_19 (26) = happyShift action_9
action_19 (27) = happyShift action_10
action_19 (29) = happyShift action_11
action_19 (30) = happyShift action_12
action_19 (31) = happyShift action_13
action_19 (33) = happyShift action_14
action_19 (36) = happyShift action_15
action_19 (41) = happyShift action_16
action_19 (50) = happyShift action_17
action_19 (55) = happyShift action_18
action_19 (62) = happyShift action_19
action_19 (70) = happyShift action_20
action_19 (5) = happyGoto action_23
action_19 (12) = happyGoto action_24
action_19 (15) = happyGoto action_3
action_19 _ = happyReduce_23

action_20 (21) = happyShift action_4
action_20 (22) = happyShift action_5
action_20 (23) = happyShift action_6
action_20 (24) = happyShift action_7
action_20 (25) = happyShift action_8
action_20 (26) = happyShift action_9
action_20 (27) = happyShift action_10
action_20 (29) = happyShift action_11
action_20 (30) = happyShift action_12
action_20 (31) = happyShift action_13
action_20 (33) = happyShift action_14
action_20 (36) = happyShift action_15
action_20 (41) = happyShift action_16
action_20 (50) = happyShift action_17
action_20 (55) = happyShift action_18
action_20 (62) = happyShift action_19
action_20 (70) = happyShift action_20
action_20 (5) = happyGoto action_22
action_20 (15) = happyGoto action_3
action_20 _ = happyFail

action_21 (78) = happyAccept
action_21 _ = happyFail

action_22 _ = happyReduce_11

action_23 (40) = happyShift action_62
action_23 _ = happyReduce_24

action_24 (63) = happyShift action_102
action_24 _ = happyFail

action_25 _ = happyReduce_50

action_26 _ = happyReduce_49

action_27 (39) = happyShift action_43
action_27 (42) = happyShift action_101
action_27 (43) = happyShift action_44
action_27 (44) = happyShift action_45
action_27 (45) = happyShift action_46
action_27 (46) = happyShift action_47
action_27 (47) = happyShift action_48
action_27 (51) = happyShift action_49
action_27 (52) = happyShift action_50
action_27 (53) = happyShift action_51
action_27 (54) = happyShift action_52
action_27 (56) = happyShift action_53
action_27 (66) = happyShift action_56
action_27 (70) = happyShift action_57
action_27 (71) = happyShift action_58
action_27 (72) = happyShift action_59
action_27 (73) = happyShift action_60
action_27 (75) = happyShift action_61
action_27 _ = happyFail

action_28 (68) = happyShift action_100
action_28 _ = happyReduce_37

action_29 _ = happyReduce_33

action_30 (28) = happyShift action_99
action_30 _ = happyFail

action_31 (48) = happyShift action_98
action_31 _ = happyReduce_13

action_32 (65) = happyShift action_97
action_32 _ = happyFail

action_33 (34) = happyShift action_96
action_33 (39) = happyShift action_43
action_33 (43) = happyShift action_44
action_33 (44) = happyShift action_45
action_33 (45) = happyShift action_46
action_33 (46) = happyShift action_47
action_33 (47) = happyShift action_48
action_33 (51) = happyShift action_49
action_33 (52) = happyShift action_50
action_33 (53) = happyShift action_51
action_33 (54) = happyShift action_52
action_33 (56) = happyShift action_53
action_33 (66) = happyShift action_56
action_33 (70) = happyShift action_57
action_33 (71) = happyShift action_58
action_33 (72) = happyShift action_59
action_33 (73) = happyShift action_60
action_33 (75) = happyShift action_61
action_33 _ = happyFail

action_34 (32) = happyShift action_94
action_34 (39) = happyShift action_43
action_34 (43) = happyShift action_44
action_34 (44) = happyShift action_45
action_34 (45) = happyShift action_46
action_34 (46) = happyShift action_47
action_34 (47) = happyShift action_48
action_34 (51) = happyShift action_49
action_34 (52) = happyShift action_50
action_34 (53) = happyShift action_51
action_34 (54) = happyShift action_52
action_34 (56) = happyShift action_53
action_34 (65) = happyShift action_95
action_34 (66) = happyShift action_56
action_34 (70) = happyShift action_57
action_34 (71) = happyShift action_58
action_34 (72) = happyShift action_59
action_34 (73) = happyShift action_60
action_34 (75) = happyShift action_61
action_34 _ = happyFail

action_35 (39) = happyShift action_43
action_35 (43) = happyShift action_44
action_35 (44) = happyShift action_45
action_35 (45) = happyShift action_46
action_35 (46) = happyShift action_47
action_35 (47) = happyShift action_48
action_35 (51) = happyShift action_49
action_35 (52) = happyShift action_50
action_35 (53) = happyShift action_51
action_35 (54) = happyShift action_52
action_35 (56) = happyShift action_53
action_35 (70) = happyShift action_57
action_35 (71) = happyShift action_58
action_35 (72) = happyShift action_59
action_35 (73) = happyShift action_60
action_35 (75) = happyShift action_61
action_35 _ = happyReduce_58

action_36 (39) = happyShift action_43
action_36 (43) = happyShift action_44
action_36 (44) = happyShift action_45
action_36 (45) = happyShift action_46
action_36 (46) = happyShift action_47
action_36 (47) = happyShift action_48
action_36 (51) = happyShift action_49
action_36 (52) = happyShift action_50
action_36 (53) = happyShift action_51
action_36 (54) = happyShift action_52
action_36 (56) = happyShift action_53
action_36 (70) = happyShift action_57
action_36 (71) = happyShift action_58
action_36 (72) = happyShift action_59
action_36 (73) = happyShift action_60
action_36 (75) = happyShift action_61
action_36 _ = happyReduce_57

action_37 (37) = happyShift action_91
action_37 (39) = happyShift action_92
action_37 (66) = happyShift action_93
action_37 _ = happyFail

action_38 (68) = happyShift action_86
action_38 (17) = happyGoto action_90
action_38 _ = happyReduce_64

action_39 (25) = happyShift action_38
action_39 (41) = happyShift action_39
action_39 (42) = happyShift action_89
action_39 (74) = happyShift action_40
action_39 (14) = happyGoto action_88
action_39 _ = happyFail

action_40 (68) = happyShift action_86
action_40 (17) = happyGoto action_87
action_40 _ = happyReduce_64

action_41 (39) = happyShift action_43
action_41 (43) = happyShift action_44
action_41 (44) = happyShift action_45
action_41 (45) = happyShift action_46
action_41 (46) = happyShift action_47
action_41 (47) = happyShift action_48
action_41 (51) = happyShift action_49
action_41 (52) = happyShift action_50
action_41 (53) = happyShift action_51
action_41 (54) = happyShift action_52
action_41 (56) = happyShift action_53
action_41 (70) = happyShift action_57
action_41 (71) = happyShift action_58
action_41 (72) = happyShift action_59
action_41 (73) = happyShift action_60
action_41 (75) = happyShift action_61
action_41 _ = happyReduce_61

action_42 (68) = happyShift action_86
action_42 (17) = happyGoto action_85
action_42 _ = happyReduce_64

action_43 (22) = happyShift action_5
action_43 (23) = happyShift action_6
action_43 (24) = happyShift action_7
action_43 (25) = happyShift action_8
action_43 (26) = happyShift action_9
action_43 (29) = happyShift action_11
action_43 (30) = happyShift action_12
action_43 (41) = happyShift action_16
action_43 (50) = happyShift action_17
action_43 (55) = happyShift action_18
action_43 (15) = happyGoto action_84
action_43 _ = happyFail

action_44 (22) = happyShift action_5
action_44 (23) = happyShift action_6
action_44 (24) = happyShift action_7
action_44 (25) = happyShift action_8
action_44 (26) = happyShift action_9
action_44 (29) = happyShift action_11
action_44 (30) = happyShift action_12
action_44 (41) = happyShift action_16
action_44 (50) = happyShift action_17
action_44 (55) = happyShift action_18
action_44 (15) = happyGoto action_83
action_44 _ = happyFail

action_45 (22) = happyShift action_5
action_45 (23) = happyShift action_6
action_45 (24) = happyShift action_7
action_45 (25) = happyShift action_8
action_45 (26) = happyShift action_9
action_45 (29) = happyShift action_11
action_45 (30) = happyShift action_12
action_45 (41) = happyShift action_16
action_45 (50) = happyShift action_17
action_45 (55) = happyShift action_18
action_45 (15) = happyGoto action_82
action_45 _ = happyFail

action_46 (22) = happyShift action_5
action_46 (23) = happyShift action_6
action_46 (24) = happyShift action_7
action_46 (25) = happyShift action_8
action_46 (26) = happyShift action_9
action_46 (29) = happyShift action_11
action_46 (30) = happyShift action_12
action_46 (41) = happyShift action_16
action_46 (50) = happyShift action_17
action_46 (55) = happyShift action_18
action_46 (15) = happyGoto action_81
action_46 _ = happyFail

action_47 (22) = happyShift action_5
action_47 (23) = happyShift action_6
action_47 (24) = happyShift action_7
action_47 (25) = happyShift action_8
action_47 (26) = happyShift action_9
action_47 (29) = happyShift action_11
action_47 (30) = happyShift action_12
action_47 (41) = happyShift action_16
action_47 (50) = happyShift action_17
action_47 (55) = happyShift action_18
action_47 (15) = happyGoto action_80
action_47 _ = happyFail

action_48 (22) = happyShift action_5
action_48 (23) = happyShift action_6
action_48 (24) = happyShift action_7
action_48 (25) = happyShift action_8
action_48 (26) = happyShift action_9
action_48 (29) = happyShift action_11
action_48 (30) = happyShift action_12
action_48 (41) = happyShift action_16
action_48 (50) = happyShift action_17
action_48 (55) = happyShift action_18
action_48 (15) = happyGoto action_79
action_48 _ = happyFail

action_49 (22) = happyShift action_5
action_49 (23) = happyShift action_6
action_49 (24) = happyShift action_7
action_49 (25) = happyShift action_8
action_49 (26) = happyShift action_9
action_49 (29) = happyShift action_11
action_49 (30) = happyShift action_12
action_49 (41) = happyShift action_16
action_49 (50) = happyShift action_17
action_49 (55) = happyShift action_18
action_49 (15) = happyGoto action_78
action_49 _ = happyFail

action_50 (22) = happyShift action_5
action_50 (23) = happyShift action_6
action_50 (24) = happyShift action_7
action_50 (25) = happyShift action_8
action_50 (26) = happyShift action_9
action_50 (29) = happyShift action_11
action_50 (30) = happyShift action_12
action_50 (41) = happyShift action_16
action_50 (50) = happyShift action_17
action_50 (55) = happyShift action_18
action_50 (15) = happyGoto action_77
action_50 _ = happyFail

action_51 (22) = happyShift action_5
action_51 (23) = happyShift action_6
action_51 (24) = happyShift action_7
action_51 (25) = happyShift action_8
action_51 (26) = happyShift action_9
action_51 (29) = happyShift action_11
action_51 (30) = happyShift action_12
action_51 (41) = happyShift action_16
action_51 (50) = happyShift action_17
action_51 (55) = happyShift action_18
action_51 (15) = happyGoto action_76
action_51 _ = happyFail

action_52 (22) = happyShift action_5
action_52 (23) = happyShift action_6
action_52 (24) = happyShift action_7
action_52 (25) = happyShift action_8
action_52 (26) = happyShift action_9
action_52 (29) = happyShift action_11
action_52 (30) = happyShift action_12
action_52 (41) = happyShift action_16
action_52 (50) = happyShift action_17
action_52 (55) = happyShift action_18
action_52 (15) = happyGoto action_75
action_52 _ = happyFail

action_53 (22) = happyShift action_5
action_53 (23) = happyShift action_6
action_53 (24) = happyShift action_7
action_53 (25) = happyShift action_8
action_53 (26) = happyShift action_9
action_53 (29) = happyShift action_11
action_53 (30) = happyShift action_12
action_53 (41) = happyShift action_16
action_53 (50) = happyShift action_17
action_53 (55) = happyShift action_18
action_53 (15) = happyGoto action_74
action_53 _ = happyFail

action_54 (22) = happyShift action_5
action_54 (23) = happyShift action_6
action_54 (24) = happyShift action_7
action_54 (25) = happyShift action_8
action_54 (26) = happyShift action_9
action_54 (29) = happyShift action_11
action_54 (30) = happyShift action_12
action_54 (41) = happyShift action_16
action_54 (50) = happyShift action_17
action_54 (55) = happyShift action_18
action_54 (15) = happyGoto action_72
action_54 (16) = happyGoto action_73
action_54 _ = happyReduce_62

action_55 (25) = happyShift action_38
action_55 (41) = happyShift action_39
action_55 (74) = happyShift action_40
action_55 (13) = happyGoto action_70
action_55 (14) = happyGoto action_71
action_55 _ = happyReduce_25

action_56 (22) = happyShift action_5
action_56 (23) = happyShift action_6
action_56 (24) = happyShift action_7
action_56 (25) = happyShift action_8
action_56 (26) = happyShift action_9
action_56 (29) = happyShift action_11
action_56 (30) = happyShift action_12
action_56 (41) = happyShift action_16
action_56 (50) = happyShift action_17
action_56 (55) = happyShift action_18
action_56 (15) = happyGoto action_69
action_56 _ = happyFail

action_57 (22) = happyShift action_5
action_57 (23) = happyShift action_6
action_57 (24) = happyShift action_7
action_57 (25) = happyShift action_8
action_57 (26) = happyShift action_9
action_57 (29) = happyShift action_11
action_57 (30) = happyShift action_12
action_57 (41) = happyShift action_16
action_57 (50) = happyShift action_17
action_57 (55) = happyShift action_18
action_57 (15) = happyGoto action_68
action_57 _ = happyFail

action_58 (22) = happyShift action_5
action_58 (23) = happyShift action_6
action_58 (24) = happyShift action_7
action_58 (25) = happyShift action_8
action_58 (26) = happyShift action_9
action_58 (29) = happyShift action_11
action_58 (30) = happyShift action_12
action_58 (41) = happyShift action_16
action_58 (50) = happyShift action_17
action_58 (55) = happyShift action_18
action_58 (15) = happyGoto action_67
action_58 _ = happyFail

action_59 (22) = happyShift action_5
action_59 (23) = happyShift action_6
action_59 (24) = happyShift action_7
action_59 (25) = happyShift action_8
action_59 (26) = happyShift action_9
action_59 (29) = happyShift action_11
action_59 (30) = happyShift action_12
action_59 (41) = happyShift action_16
action_59 (50) = happyShift action_17
action_59 (55) = happyShift action_18
action_59 (15) = happyGoto action_66
action_59 _ = happyFail

action_60 (22) = happyShift action_5
action_60 (23) = happyShift action_6
action_60 (24) = happyShift action_7
action_60 (25) = happyShift action_8
action_60 (26) = happyShift action_9
action_60 (29) = happyShift action_11
action_60 (30) = happyShift action_12
action_60 (41) = happyShift action_16
action_60 (50) = happyShift action_17
action_60 (55) = happyShift action_18
action_60 (15) = happyGoto action_65
action_60 _ = happyFail

action_61 (22) = happyShift action_5
action_61 (23) = happyShift action_6
action_61 (24) = happyShift action_7
action_61 (25) = happyShift action_8
action_61 (26) = happyShift action_9
action_61 (29) = happyShift action_11
action_61 (30) = happyShift action_12
action_61 (41) = happyShift action_16
action_61 (50) = happyShift action_17
action_61 (55) = happyShift action_18
action_61 (15) = happyGoto action_64
action_61 _ = happyFail

action_62 (21) = happyShift action_4
action_62 (22) = happyShift action_5
action_62 (23) = happyShift action_6
action_62 (24) = happyShift action_7
action_62 (25) = happyShift action_8
action_62 (26) = happyShift action_9
action_62 (27) = happyShift action_10
action_62 (29) = happyShift action_11
action_62 (30) = happyShift action_12
action_62 (31) = happyShift action_13
action_62 (33) = happyShift action_14
action_62 (36) = happyShift action_15
action_62 (41) = happyShift action_16
action_62 (50) = happyShift action_17
action_62 (55) = happyShift action_18
action_62 (62) = happyShift action_19
action_62 (70) = happyShift action_20
action_62 (5) = happyGoto action_63
action_62 (15) = happyGoto action_3
action_62 _ = happyFail

action_63 _ = happyReduce_8

action_64 (75) = happyShift action_61
action_64 _ = happyReduce_54

action_65 (75) = happyShift action_61
action_65 _ = happyReduce_55

action_66 (56) = happyShift action_53
action_66 (70) = happyShift action_57
action_66 (73) = happyShift action_60
action_66 (75) = happyShift action_61
action_66 _ = happyReduce_52

action_67 (56) = happyShift action_53
action_67 (70) = happyShift action_57
action_67 (73) = happyShift action_60
action_67 (75) = happyShift action_61
action_67 _ = happyReduce_51

action_68 (75) = happyShift action_61
action_68 _ = happyReduce_53

action_69 (39) = happyShift action_43
action_69 (43) = happyShift action_44
action_69 (44) = happyShift action_45
action_69 (45) = happyShift action_46
action_69 (46) = happyShift action_47
action_69 (47) = happyShift action_48
action_69 (51) = happyShift action_49
action_69 (52) = happyShift action_50
action_69 (53) = happyShift action_51
action_69 (54) = happyShift action_52
action_69 (56) = happyShift action_53
action_69 (66) = happyShift action_56
action_69 (70) = happyShift action_57
action_69 (71) = happyShift action_58
action_69 (72) = happyShift action_59
action_69 (73) = happyShift action_60
action_69 (75) = happyShift action_61
action_69 _ = happyReduce_60

action_70 (67) = happyShift action_123
action_70 (11) = happyGoto action_124
action_70 _ = happyReduce_21

action_71 (37) = happyShift action_91
action_71 (66) = happyShift action_93
action_71 _ = happyReduce_26

action_72 (39) = happyShift action_43
action_72 (43) = happyShift action_44
action_72 (44) = happyShift action_45
action_72 (45) = happyShift action_46
action_72 (46) = happyShift action_47
action_72 (47) = happyShift action_48
action_72 (51) = happyShift action_49
action_72 (52) = happyShift action_50
action_72 (53) = happyShift action_51
action_72 (54) = happyShift action_52
action_72 (56) = happyShift action_53
action_72 (66) = happyShift action_56
action_72 (70) = happyShift action_57
action_72 (71) = happyShift action_58
action_72 (72) = happyShift action_59
action_72 (73) = happyShift action_60
action_72 (75) = happyShift action_61
action_72 _ = happyReduce_63

action_73 (67) = happyShift action_123
action_73 (11) = happyGoto action_122
action_73 _ = happyReduce_21

action_74 (75) = happyShift action_61
action_74 _ = happyReduce_56

action_75 (39) = happyShift action_43
action_75 (43) = happyShift action_44
action_75 (44) = happyShift action_45
action_75 (45) = happyShift action_46
action_75 (46) = happyShift action_47
action_75 (47) = happyShift action_48
action_75 (56) = happyShift action_53
action_75 (70) = happyShift action_57
action_75 (71) = happyShift action_58
action_75 (72) = happyShift action_59
action_75 (73) = happyShift action_60
action_75 (75) = happyShift action_61
action_75 _ = happyReduce_41

action_76 (39) = happyShift action_43
action_76 (43) = happyShift action_44
action_76 (44) = happyShift action_45
action_76 (45) = happyShift action_46
action_76 (46) = happyShift action_47
action_76 (47) = happyShift action_48
action_76 (56) = happyShift action_53
action_76 (70) = happyShift action_57
action_76 (71) = happyShift action_58
action_76 (72) = happyShift action_59
action_76 (73) = happyShift action_60
action_76 (75) = happyShift action_61
action_76 _ = happyReduce_42

action_77 (39) = happyShift action_43
action_77 (43) = happyShift action_44
action_77 (44) = happyShift action_45
action_77 (45) = happyShift action_46
action_77 (46) = happyShift action_47
action_77 (47) = happyShift action_48
action_77 (56) = happyShift action_53
action_77 (70) = happyShift action_57
action_77 (71) = happyShift action_58
action_77 (72) = happyShift action_59
action_77 (73) = happyShift action_60
action_77 (75) = happyShift action_61
action_77 _ = happyReduce_39

action_78 (39) = happyShift action_43
action_78 (43) = happyShift action_44
action_78 (44) = happyShift action_45
action_78 (45) = happyShift action_46
action_78 (46) = happyShift action_47
action_78 (47) = happyShift action_48
action_78 (56) = happyShift action_53
action_78 (70) = happyShift action_57
action_78 (71) = happyShift action_58
action_78 (72) = happyShift action_59
action_78 (73) = happyShift action_60
action_78 (75) = happyShift action_61
action_78 _ = happyReduce_40

action_79 (39) = happyFail
action_79 (43) = happyFail
action_79 (44) = happyFail
action_79 (45) = happyFail
action_79 (46) = happyFail
action_79 (47) = happyFail
action_79 (56) = happyShift action_53
action_79 (70) = happyShift action_57
action_79 (71) = happyShift action_58
action_79 (72) = happyShift action_59
action_79 (73) = happyShift action_60
action_79 (75) = happyShift action_61
action_79 _ = happyReduce_48

action_80 (39) = happyFail
action_80 (43) = happyFail
action_80 (44) = happyFail
action_80 (45) = happyFail
action_80 (46) = happyFail
action_80 (47) = happyFail
action_80 (56) = happyShift action_53
action_80 (70) = happyShift action_57
action_80 (71) = happyShift action_58
action_80 (72) = happyShift action_59
action_80 (73) = happyShift action_60
action_80 (75) = happyShift action_61
action_80 _ = happyReduce_47

action_81 (39) = happyFail
action_81 (43) = happyFail
action_81 (44) = happyFail
action_81 (45) = happyFail
action_81 (46) = happyFail
action_81 (47) = happyFail
action_81 (56) = happyShift action_53
action_81 (70) = happyShift action_57
action_81 (71) = happyShift action_58
action_81 (72) = happyShift action_59
action_81 (73) = happyShift action_60
action_81 (75) = happyShift action_61
action_81 _ = happyReduce_46

action_82 (39) = happyFail
action_82 (43) = happyFail
action_82 (44) = happyFail
action_82 (45) = happyFail
action_82 (46) = happyFail
action_82 (47) = happyFail
action_82 (56) = happyShift action_53
action_82 (70) = happyShift action_57
action_82 (71) = happyShift action_58
action_82 (72) = happyShift action_59
action_82 (73) = happyShift action_60
action_82 (75) = happyShift action_61
action_82 _ = happyReduce_44

action_83 (39) = happyFail
action_83 (43) = happyFail
action_83 (44) = happyFail
action_83 (45) = happyFail
action_83 (46) = happyFail
action_83 (47) = happyFail
action_83 (56) = happyShift action_53
action_83 (70) = happyShift action_57
action_83 (71) = happyShift action_58
action_83 (72) = happyShift action_59
action_83 (73) = happyShift action_60
action_83 (75) = happyShift action_61
action_83 _ = happyReduce_43

action_84 (39) = happyFail
action_84 (43) = happyFail
action_84 (44) = happyFail
action_84 (45) = happyFail
action_84 (46) = happyFail
action_84 (47) = happyFail
action_84 (56) = happyShift action_53
action_84 (70) = happyShift action_57
action_84 (71) = happyShift action_58
action_84 (72) = happyShift action_59
action_84 (73) = happyShift action_60
action_84 (75) = happyShift action_61
action_84 _ = happyReduce_45

action_85 (28) = happyShift action_121
action_85 _ = happyFail

action_86 (57) = happyShift action_105
action_86 (58) = happyShift action_106
action_86 (59) = happyShift action_107
action_86 (60) = happyShift action_108
action_86 (74) = happyShift action_109
action_86 (18) = happyGoto action_103
action_86 (19) = happyGoto action_120
action_86 _ = happyFail

action_87 _ = happyReduce_28

action_88 (37) = happyShift action_91
action_88 (42) = happyShift action_119
action_88 (66) = happyShift action_93
action_88 _ = happyFail

action_89 _ = happyReduce_27

action_90 _ = happyReduce_29

action_91 (25) = happyShift action_118
action_91 _ = happyFail

action_92 (22) = happyShift action_5
action_92 (23) = happyShift action_6
action_92 (24) = happyShift action_7
action_92 (25) = happyShift action_8
action_92 (26) = happyShift action_9
action_92 (29) = happyShift action_11
action_92 (30) = happyShift action_12
action_92 (41) = happyShift action_16
action_92 (50) = happyShift action_17
action_92 (55) = happyShift action_18
action_92 (15) = happyGoto action_117
action_92 _ = happyFail

action_93 (25) = happyShift action_38
action_93 (41) = happyShift action_39
action_93 (74) = happyShift action_40
action_93 (14) = happyGoto action_116
action_93 _ = happyFail

action_94 (62) = happyShift action_115
action_94 _ = happyFail

action_95 (32) = happyShift action_114
action_95 _ = happyFail

action_96 (21) = happyShift action_4
action_96 (22) = happyShift action_5
action_96 (23) = happyShift action_6
action_96 (24) = happyShift action_7
action_96 (25) = happyShift action_8
action_96 (26) = happyShift action_9
action_96 (27) = happyShift action_10
action_96 (29) = happyShift action_11
action_96 (30) = happyShift action_12
action_96 (31) = happyShift action_13
action_96 (33) = happyShift action_14
action_96 (36) = happyShift action_15
action_96 (41) = happyShift action_16
action_96 (50) = happyShift action_17
action_96 (55) = happyShift action_18
action_96 (62) = happyShift action_19
action_96 (70) = happyShift action_20
action_96 (5) = happyGoto action_113
action_96 (15) = happyGoto action_3
action_96 _ = happyFail

action_97 (25) = happyShift action_38
action_97 (41) = happyShift action_39
action_97 (74) = happyShift action_40
action_97 (13) = happyGoto action_112
action_97 (14) = happyGoto action_71
action_97 _ = happyReduce_25

action_98 (25) = happyShift action_32
action_98 (6) = happyGoto action_111
action_98 (7) = happyGoto action_31
action_98 _ = happyFail

action_99 (21) = happyShift action_4
action_99 (22) = happyShift action_5
action_99 (23) = happyShift action_6
action_99 (24) = happyShift action_7
action_99 (25) = happyShift action_8
action_99 (26) = happyShift action_9
action_99 (27) = happyShift action_10
action_99 (29) = happyShift action_11
action_99 (30) = happyShift action_12
action_99 (31) = happyShift action_13
action_99 (33) = happyShift action_14
action_99 (36) = happyShift action_15
action_99 (41) = happyShift action_16
action_99 (50) = happyShift action_17
action_99 (55) = happyShift action_18
action_99 (62) = happyShift action_19
action_99 (70) = happyShift action_20
action_99 (5) = happyGoto action_110
action_99 (15) = happyGoto action_3
action_99 _ = happyFail

action_100 (57) = happyShift action_105
action_100 (58) = happyShift action_106
action_100 (59) = happyShift action_107
action_100 (60) = happyShift action_108
action_100 (74) = happyShift action_109
action_100 (18) = happyGoto action_103
action_100 (19) = happyGoto action_104
action_100 _ = happyFail

action_101 _ = happyReduce_59

action_102 _ = happyReduce_10

action_103 _ = happyReduce_69

action_104 (42) = happyShift action_139
action_104 (70) = happyShift action_127
action_104 (75) = happyShift action_128
action_104 _ = happyFail

action_105 _ = happyReduce_66

action_106 _ = happyReduce_67

action_107 _ = happyReduce_68

action_108 (57) = happyShift action_105
action_108 (58) = happyShift action_106
action_108 (59) = happyShift action_107
action_108 (60) = happyShift action_108
action_108 (74) = happyShift action_109
action_108 (18) = happyGoto action_103
action_108 (19) = happyGoto action_138
action_108 _ = happyFail

action_109 _ = happyReduce_70

action_110 _ = happyReduce_12

action_111 _ = happyReduce_14

action_112 (39) = happyShift action_137
action_112 _ = happyFail

action_113 (35) = happyShift action_136
action_113 (40) = happyShift action_62
action_113 _ = happyFail

action_114 (62) = happyShift action_135
action_114 _ = happyFail

action_115 (26) = happyShift action_134
action_115 (8) = happyGoto action_131
action_115 (9) = happyGoto action_132
action_115 (10) = happyGoto action_133
action_115 _ = happyReduce_16

action_116 (66) = happyShift action_93
action_116 _ = happyReduce_31

action_117 (28) = happyShift action_130
action_117 (39) = happyShift action_43
action_117 (43) = happyShift action_44
action_117 (44) = happyShift action_45
action_117 (45) = happyShift action_46
action_117 (46) = happyShift action_47
action_117 (47) = happyShift action_48
action_117 (51) = happyShift action_49
action_117 (52) = happyShift action_50
action_117 (53) = happyShift action_51
action_117 (54) = happyShift action_52
action_117 (56) = happyShift action_53
action_117 (66) = happyShift action_56
action_117 (70) = happyShift action_57
action_117 (71) = happyShift action_58
action_117 (72) = happyShift action_59
action_117 (73) = happyShift action_60
action_117 (75) = happyShift action_61
action_117 _ = happyFail

action_118 (68) = happyShift action_86
action_118 (17) = happyGoto action_129
action_118 _ = happyReduce_64

action_119 _ = happyReduce_32

action_120 (70) = happyShift action_127
action_120 (75) = happyShift action_128
action_120 _ = happyReduce_65

action_121 (21) = happyShift action_4
action_121 (22) = happyShift action_5
action_121 (23) = happyShift action_6
action_121 (24) = happyShift action_7
action_121 (25) = happyShift action_8
action_121 (26) = happyShift action_9
action_121 (27) = happyShift action_10
action_121 (29) = happyShift action_11
action_121 (30) = happyShift action_12
action_121 (31) = happyShift action_13
action_121 (33) = happyShift action_14
action_121 (36) = happyShift action_15
action_121 (41) = happyShift action_16
action_121 (50) = happyShift action_17
action_121 (55) = happyShift action_18
action_121 (62) = happyShift action_19
action_121 (70) = happyShift action_20
action_121 (5) = happyGoto action_126
action_121 (15) = happyGoto action_3
action_121 _ = happyFail

action_122 _ = happyReduce_2

action_123 (21) = happyShift action_4
action_123 (22) = happyShift action_5
action_123 (23) = happyShift action_6
action_123 (24) = happyShift action_7
action_123 (25) = happyShift action_8
action_123 (26) = happyShift action_9
action_123 (27) = happyShift action_10
action_123 (29) = happyShift action_11
action_123 (30) = happyShift action_12
action_123 (31) = happyShift action_13
action_123 (33) = happyShift action_14
action_123 (36) = happyShift action_15
action_123 (41) = happyShift action_16
action_123 (50) = happyShift action_17
action_123 (55) = happyShift action_18
action_123 (62) = happyShift action_19
action_123 (70) = happyShift action_20
action_123 (5) = happyGoto action_125
action_123 (15) = happyGoto action_3
action_123 _ = happyFail

action_124 _ = happyReduce_3

action_125 _ = happyReduce_22

action_126 _ = happyReduce_9

action_127 (57) = happyShift action_105
action_127 (58) = happyShift action_106
action_127 (59) = happyShift action_107
action_127 (60) = happyShift action_108
action_127 (74) = happyShift action_109
action_127 (18) = happyGoto action_103
action_127 (19) = happyGoto action_149
action_127 _ = happyFail

action_128 (57) = happyShift action_105
action_128 (58) = happyShift action_106
action_128 (59) = happyShift action_107
action_128 (60) = happyShift action_108
action_128 (74) = happyShift action_109
action_128 (18) = happyGoto action_103
action_128 (19) = happyGoto action_148
action_128 _ = happyFail

action_129 _ = happyReduce_30

action_130 (21) = happyShift action_4
action_130 (22) = happyShift action_5
action_130 (23) = happyShift action_6
action_130 (24) = happyShift action_7
action_130 (25) = happyShift action_8
action_130 (26) = happyShift action_9
action_130 (27) = happyShift action_10
action_130 (29) = happyShift action_11
action_130 (30) = happyShift action_12
action_130 (31) = happyShift action_13
action_130 (33) = happyShift action_14
action_130 (36) = happyShift action_15
action_130 (41) = happyShift action_16
action_130 (50) = happyShift action_17
action_130 (55) = happyShift action_18
action_130 (62) = happyShift action_19
action_130 (70) = happyShift action_20
action_130 (5) = happyGoto action_147
action_130 (15) = happyGoto action_3
action_130 _ = happyFail

action_131 (63) = happyShift action_146
action_131 _ = happyFail

action_132 _ = happyReduce_17

action_133 (69) = happyShift action_145
action_133 _ = happyReduce_18

action_134 (25) = happyShift action_38
action_134 (41) = happyShift action_39
action_134 (74) = happyShift action_40
action_134 (13) = happyGoto action_144
action_134 (14) = happyGoto action_71
action_134 _ = happyReduce_25

action_135 (26) = happyShift action_134
action_135 (8) = happyGoto action_143
action_135 (9) = happyGoto action_132
action_135 (10) = happyGoto action_133
action_135 _ = happyReduce_16

action_136 (21) = happyShift action_4
action_136 (22) = happyShift action_5
action_136 (23) = happyShift action_6
action_136 (24) = happyShift action_7
action_136 (25) = happyShift action_8
action_136 (26) = happyShift action_9
action_136 (27) = happyShift action_10
action_136 (29) = happyShift action_11
action_136 (30) = happyShift action_12
action_136 (31) = happyShift action_13
action_136 (33) = happyShift action_14
action_136 (36) = happyShift action_15
action_136 (41) = happyShift action_16
action_136 (50) = happyShift action_17
action_136 (55) = happyShift action_18
action_136 (62) = happyShift action_19
action_136 (70) = happyShift action_20
action_136 (5) = happyGoto action_142
action_136 (15) = happyGoto action_3
action_136 _ = happyFail

action_137 (21) = happyShift action_4
action_137 (22) = happyShift action_5
action_137 (23) = happyShift action_6
action_137 (24) = happyShift action_7
action_137 (25) = happyShift action_8
action_137 (26) = happyShift action_9
action_137 (27) = happyShift action_10
action_137 (29) = happyShift action_11
action_137 (30) = happyShift action_12
action_137 (31) = happyShift action_13
action_137 (33) = happyShift action_14
action_137 (36) = happyShift action_15
action_137 (41) = happyShift action_16
action_137 (50) = happyShift action_17
action_137 (55) = happyShift action_18
action_137 (62) = happyShift action_19
action_137 (70) = happyShift action_20
action_137 (5) = happyGoto action_141
action_137 (15) = happyGoto action_3
action_137 _ = happyFail

action_138 (61) = happyShift action_140
action_138 (70) = happyShift action_127
action_138 (75) = happyShift action_128
action_138 _ = happyFail

action_139 _ = happyReduce_38

action_140 (62) = happyShift action_153
action_140 _ = happyFail

action_141 (40) = happyShift action_62
action_141 _ = happyReduce_15

action_142 (40) = happyShift action_62
action_142 _ = happyReduce_7

action_143 (63) = happyShift action_152
action_143 _ = happyFail

action_144 (38) = happyShift action_151
action_144 _ = happyFail

action_145 (26) = happyShift action_134
action_145 (9) = happyGoto action_150
action_145 (10) = happyGoto action_133
action_145 _ = happyFail

action_146 _ = happyReduce_5

action_147 _ = happyReduce_4

action_148 (75) = happyShift action_128
action_148 _ = happyReduce_72

action_149 (75) = happyShift action_128
action_149 _ = happyReduce_73

action_150 _ = happyReduce_19

action_151 (21) = happyShift action_4
action_151 (22) = happyShift action_5
action_151 (23) = happyShift action_6
action_151 (24) = happyShift action_7
action_151 (25) = happyShift action_8
action_151 (26) = happyShift action_9
action_151 (27) = happyShift action_10
action_151 (29) = happyShift action_11
action_151 (30) = happyShift action_12
action_151 (31) = happyShift action_13
action_151 (33) = happyShift action_14
action_151 (36) = happyShift action_15
action_151 (41) = happyShift action_16
action_151 (50) = happyShift action_17
action_151 (55) = happyShift action_18
action_151 (62) = happyShift action_19
action_151 (70) = happyShift action_20
action_151 (5) = happyGoto action_157
action_151 (15) = happyGoto action_3
action_151 _ = happyFail

action_152 _ = happyReduce_6

action_153 (22) = happyShift action_155
action_153 (77) = happyShift action_156
action_153 (20) = happyGoto action_154
action_153 _ = happyFail

action_154 (66) = happyShift action_158
action_154 _ = happyFail

action_155 _ = happyReduce_74

action_156 _ = happyReduce_75

action_157 (40) = happyShift action_62
action_157 _ = happyReduce_20

action_158 (22) = happyShift action_155
action_158 (77) = happyShift action_156
action_158 (20) = happyGoto action_159
action_158 _ = happyFail

action_159 (63) = happyShift action_160
action_159 _ = happyFail

action_160 _ = happyReduce_71

happyReduce_1 = happySpecReduce_1  4 happyReduction_1
happyReduction_1 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_1 _  = notHappyAtAll 

happyReduce_2 = happyReduce 4 5 happyReduction_2
happyReduction_2 ((HappyAbsSyn11  happy_var_4) `HappyStk`
	(HappyAbsSyn16  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn15  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (Send happy_var_1 happy_var_3 happy_var_4
	) `HappyStk` happyRest

happyReduce_3 = happyReduce 4 5 happyReduction_3
happyReduction_3 ((HappyAbsSyn11  happy_var_4) `HappyStk`
	(HappyAbsSyn13  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn15  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (let (x, t) = getBinding happy_var_3 in
            Receive happy_var_1 x t $ expand (Id x t) happy_var_3 happy_var_4
	) `HappyStk` happyRest

happyReduce_4 = happyReduce 6 5 happyReduction_4
happyReduction_4 ((HappyAbsSyn5  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn15  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn14  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (expand happy_var_4 happy_var_2 happy_var_6
	) `HappyStk` happyRest

happyReduce_5 = happyReduce 6 5 happyReduction_5
happyReduction_5 (_ `HappyStk`
	(HappyAbsSyn8  happy_var_5) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn15  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (Case happy_var_2 happy_var_5
	) `HappyStk` happyRest

happyReduce_6 = happyReduce 7 5 happyReduction_6
happyReduction_6 (_ `HappyStk`
	(HappyAbsSyn8  happy_var_6) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn15  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (Receive happy_var_2 "%val" Nothing $ Case (Id "%val" Nothing) happy_var_6
	) `HappyStk` happyRest

happyReduce_7 = happyReduce 6 5 happyReduction_7
happyReduction_7 ((HappyAbsSyn5  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn15  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (If happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_8 = happySpecReduce_3  5 happyReduction_8
happyReduction_8 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (Par happy_var_1 happy_var_3
	)
happyReduction_8 _ _ _  = notHappyAtAll 

happyReduce_9 = happyReduce 5 5 happyReduction_9
happyReduction_9 ((HappyAbsSyn5  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn17  happy_var_3) `HappyStk`
	(HappyTerminal (LID happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (New happy_var_2 happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_10 = happySpecReduce_3  5 happyReduction_10
happyReduction_10 _
	(HappyAbsSyn12  happy_var_2)
	_
	 =  HappyAbsSyn5
		 (Group happy_var_2
	)
happyReduction_10 _ _ _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_2  5 happyReduction_11
happyReduction_11 (HappyAbsSyn5  happy_var_2)
	_
	 =  HappyAbsSyn5
		 (Star happy_var_2
	)
happyReduction_11 _ _  = notHappyAtAll 

happyReduce_12 = happyReduce 4 5 happyReduction_12
happyReduction_12 ((HappyAbsSyn5  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (let t = TChannel tunknown (C.singleton Omega) (C.singleton Omega) in
            let s = TChannel tunknown C.empty (C.singleton Omega) in
            let env = M.fromList $ Prelude.map (\(x, _) -> (x, Just s)) happy_var_2 in
            foldr (\var -> New var (Just t))
                  (foldr Par (Process.assignType env happy_var_4) (Prelude.map (Process.assignType env . snd) happy_var_2))
                  (Prelude.map fst happy_var_2)
	) `HappyStk` happyRest

happyReduce_13 = happySpecReduce_1  6 happyReduction_13
happyReduction_13 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn6
		 ([happy_var_1]
	)
happyReduction_13 _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_3  6 happyReduction_14
happyReduction_14 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1 : happy_var_3
	)
happyReduction_14 _ _ _  = notHappyAtAll 

happyReduce_15 = happyReduce 5 7 happyReduction_15
happyReduction_15 ((HappyAbsSyn5  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn13  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (LID happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (let (x, t) = getBinding happy_var_3 in
             (happy_var_1, Star $ Receive (Id happy_var_1 (Just $ TChannel tunknown (C.singleton Omega) (C.singleton Omega))) x t $ expand (Id x t) happy_var_3 happy_var_5)
	) `HappyStk` happyRest

happyReduce_16 = happySpecReduce_0  8 happyReduction_16
happyReduction_16  =  HappyAbsSyn8
		 ([]
	)

happyReduce_17 = happySpecReduce_1  8 happyReduction_17
happyReduction_17 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_1
	)
happyReduction_17 _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_1  9 happyReduction_18
happyReduction_18 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn9
		 ([happy_var_1]
	)
happyReduction_18 _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_3  9 happyReduction_19
happyReduction_19 (HappyAbsSyn9  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_1 : happy_var_3
	)
happyReduction_19 _ _ _  = notHappyAtAll 

happyReduce_20 = happyReduce 4 10 happyReduction_20
happyReduction_20 ((HappyAbsSyn5  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn13  happy_var_2) `HappyStk`
	(HappyTerminal (UID happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn10
		 (let (x, t) = getBinding happy_var_2 in
              (happy_var_1, x, t, expand (Id x t) happy_var_2 happy_var_4)
	) `HappyStk` happyRest

happyReduce_21 = happySpecReduce_0  11 happyReduction_21
happyReduction_21  =  HappyAbsSyn11
		 (Idle
	)

happyReduce_22 = happySpecReduce_2  11 happyReduction_22
happyReduction_22 (HappyAbsSyn5  happy_var_2)
	_
	 =  HappyAbsSyn11
		 (happy_var_2
	)
happyReduction_22 _ _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_0  12 happyReduction_23
happyReduction_23  =  HappyAbsSyn12
		 (Idle
	)

happyReduce_24 = happySpecReduce_1  12 happyReduction_24
happyReduction_24 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn12
		 (happy_var_1
	)
happyReduction_24 _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_0  13 happyReduction_25
happyReduction_25  =  HappyAbsSyn13
		 (PUnit
	)

happyReduce_26 = happySpecReduce_1  13 happyReduction_26
happyReduction_26 (HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn13
		 (happy_var_1
	)
happyReduction_26 _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_2  14 happyReduction_27
happyReduction_27 _
	_
	 =  HappyAbsSyn14
		 (PUnit
	)

happyReduce_28 = happySpecReduce_2  14 happyReduction_28
happyReduction_28 (HappyAbsSyn17  happy_var_2)
	_
	 =  HappyAbsSyn14
		 (PAny happy_var_2
	)
happyReduction_28 _ _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_2  14 happyReduction_29
happyReduction_29 (HappyAbsSyn17  happy_var_2)
	(HappyTerminal (LID happy_var_1))
	 =  HappyAbsSyn14
		 (PVar happy_var_1 happy_var_2
	)
happyReduction_29 _ _  = notHappyAtAll 

happyReduce_30 = happyReduce 4 14 happyReduction_30
happyReduction_30 ((HappyAbsSyn17  happy_var_4) `HappyStk`
	(HappyTerminal (LID happy_var_3)) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn14  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn14
		 (PAs happy_var_1 happy_var_3 happy_var_4
	) `HappyStk` happyRest

happyReduce_31 = happySpecReduce_3  14 happyReduction_31
happyReduction_31 (HappyAbsSyn14  happy_var_3)
	_
	(HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn14
		 (PPair happy_var_1 happy_var_3
	)
happyReduction_31 _ _ _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_3  14 happyReduction_32
happyReduction_32 _
	(HappyAbsSyn14  happy_var_2)
	_
	 =  HappyAbsSyn14
		 (happy_var_2
	)
happyReduction_32 _ _ _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_2  15 happyReduction_33
happyReduction_33 _
	_
	 =  HappyAbsSyn15
		 (Const UNIT
	)

happyReduce_34 = happySpecReduce_1  15 happyReduction_34
happyReduction_34 (HappyTerminal (Lexer.INT happy_var_1))
	 =  HappyAbsSyn15
		 (Const $ Process.INT happy_var_1
	)
happyReduction_34 _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_1  15 happyReduction_35
happyReduction_35 _
	 =  HappyAbsSyn15
		 (Const $ Process.BOOL True
	)

happyReduce_36 = happySpecReduce_1  15 happyReduction_36
happyReduction_36 _
	 =  HappyAbsSyn15
		 (Const $ Process.BOOL False
	)

happyReduce_37 = happySpecReduce_1  15 happyReduction_37
happyReduction_37 (HappyTerminal (LID happy_var_1))
	 =  HappyAbsSyn15
		 (Id happy_var_1 Nothing
	)
happyReduction_37 _  = notHappyAtAll 

happyReduce_38 = happyReduce 5 15 happyReduction_38
happyReduction_38 (_ `HappyStk`
	(HappyAbsSyn19  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (LID happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn15
		 (Id happy_var_2 (Just happy_var_4)
	) `HappyStk` happyRest

happyReduce_39 = happySpecReduce_3  15 happyReduction_39
happyReduction_39 (HappyAbsSyn15  happy_var_3)
	_
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn15
		 (BinOp Process.AND happy_var_1 happy_var_3
	)
happyReduction_39 _ _ _  = notHappyAtAll 

happyReduce_40 = happySpecReduce_3  15 happyReduction_40
happyReduction_40 (HappyAbsSyn15  happy_var_3)
	_
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn15
		 (BinOp Process.AND happy_var_1 happy_var_3
	)
happyReduction_40 _ _ _  = notHappyAtAll 

happyReduce_41 = happySpecReduce_3  15 happyReduction_41
happyReduction_41 (HappyAbsSyn15  happy_var_3)
	_
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn15
		 (BinOp Process.OR happy_var_1 happy_var_3
	)
happyReduction_41 _ _ _  = notHappyAtAll 

happyReduce_42 = happySpecReduce_3  15 happyReduction_42
happyReduction_42 (HappyAbsSyn15  happy_var_3)
	_
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn15
		 (BinOp Process.OR happy_var_1 happy_var_3
	)
happyReduction_42 _ _ _  = notHappyAtAll 

happyReduce_43 = happySpecReduce_3  15 happyReduction_43
happyReduction_43 (HappyAbsSyn15  happy_var_3)
	_
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn15
		 (BinOp Process.LT happy_var_1 happy_var_3
	)
happyReduction_43 _ _ _  = notHappyAtAll 

happyReduce_44 = happySpecReduce_3  15 happyReduction_44
happyReduction_44 (HappyAbsSyn15  happy_var_3)
	_
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn15
		 (BinOp Process.GT happy_var_1 happy_var_3
	)
happyReduction_44 _ _ _  = notHappyAtAll 

happyReduce_45 = happySpecReduce_3  15 happyReduction_45
happyReduction_45 (HappyAbsSyn15  happy_var_3)
	_
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn15
		 (BinOp Process.EQ happy_var_1 happy_var_3
	)
happyReduction_45 _ _ _  = notHappyAtAll 

happyReduce_46 = happySpecReduce_3  15 happyReduction_46
happyReduction_46 (HappyAbsSyn15  happy_var_3)
	_
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn15
		 (BinOp Process.LE happy_var_1 happy_var_3
	)
happyReduction_46 _ _ _  = notHappyAtAll 

happyReduce_47 = happySpecReduce_3  15 happyReduction_47
happyReduction_47 (HappyAbsSyn15  happy_var_3)
	_
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn15
		 (BinOp Process.GE happy_var_1 happy_var_3
	)
happyReduction_47 _ _ _  = notHappyAtAll 

happyReduce_48 = happySpecReduce_3  15 happyReduction_48
happyReduction_48 (HappyAbsSyn15  happy_var_3)
	_
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn15
		 (BinOp Process.NE happy_var_1 happy_var_3
	)
happyReduction_48 _ _ _  = notHappyAtAll 

happyReduce_49 = happySpecReduce_2  15 happyReduction_49
happyReduction_49 (HappyAbsSyn15  happy_var_2)
	_
	 =  HappyAbsSyn15
		 (UnOp Process.NOT happy_var_2
	)
happyReduction_49 _ _  = notHappyAtAll 

happyReduce_50 = happySpecReduce_2  15 happyReduction_50
happyReduction_50 (HappyAbsSyn15  happy_var_2)
	_
	 =  HappyAbsSyn15
		 (UnOp Process.NOT happy_var_2
	)
happyReduction_50 _ _  = notHappyAtAll 

happyReduce_51 = happySpecReduce_3  15 happyReduction_51
happyReduction_51 (HappyAbsSyn15  happy_var_3)
	_
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn15
		 (BinOp Process.ADD happy_var_1 happy_var_3
	)
happyReduction_51 _ _ _  = notHappyAtAll 

happyReduce_52 = happySpecReduce_3  15 happyReduction_52
happyReduction_52 (HappyAbsSyn15  happy_var_3)
	_
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn15
		 (BinOp Process.SUB happy_var_1 happy_var_3
	)
happyReduction_52 _ _ _  = notHappyAtAll 

happyReduce_53 = happySpecReduce_3  15 happyReduction_53
happyReduction_53 (HappyAbsSyn15  happy_var_3)
	_
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn15
		 (BinOp Process.MUL happy_var_1 happy_var_3
	)
happyReduction_53 _ _ _  = notHappyAtAll 

happyReduce_54 = happySpecReduce_3  15 happyReduction_54
happyReduction_54 (HappyAbsSyn15  happy_var_3)
	_
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn15
		 (BinOp Process.MUL happy_var_1 happy_var_3
	)
happyReduction_54 _ _ _  = notHappyAtAll 

happyReduce_55 = happySpecReduce_3  15 happyReduction_55
happyReduction_55 (HappyAbsSyn15  happy_var_3)
	_
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn15
		 (BinOp Process.DIV happy_var_1 happy_var_3
	)
happyReduction_55 _ _ _  = notHappyAtAll 

happyReduce_56 = happySpecReduce_3  15 happyReduction_56
happyReduction_56 (HappyAbsSyn15  happy_var_3)
	_
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn15
		 (BinOp Process.MOD happy_var_1 happy_var_3
	)
happyReduction_56 _ _ _  = notHappyAtAll 

happyReduce_57 = happySpecReduce_2  15 happyReduction_57
happyReduction_57 (HappyAbsSyn15  happy_var_2)
	_
	 =  HappyAbsSyn15
		 (UnOp Process.FST happy_var_2
	)
happyReduction_57 _ _  = notHappyAtAll 

happyReduce_58 = happySpecReduce_2  15 happyReduction_58
happyReduction_58 (HappyAbsSyn15  happy_var_2)
	_
	 =  HappyAbsSyn15
		 (UnOp Process.SND happy_var_2
	)
happyReduction_58 _ _  = notHappyAtAll 

happyReduce_59 = happySpecReduce_3  15 happyReduction_59
happyReduction_59 _
	(HappyAbsSyn15  happy_var_2)
	_
	 =  HappyAbsSyn15
		 (happy_var_2
	)
happyReduction_59 _ _ _  = notHappyAtAll 

happyReduce_60 = happySpecReduce_3  15 happyReduction_60
happyReduction_60 (HappyAbsSyn15  happy_var_3)
	_
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn15
		 (BinOp PAIR happy_var_1 happy_var_3
	)
happyReduction_60 _ _ _  = notHappyAtAll 

happyReduce_61 = happySpecReduce_2  15 happyReduction_61
happyReduction_61 (HappyAbsSyn15  happy_var_2)
	(HappyTerminal (UID happy_var_1))
	 =  HappyAbsSyn15
		 (Tag happy_var_1 happy_var_2
	)
happyReduction_61 _ _  = notHappyAtAll 

happyReduce_62 = happySpecReduce_0  16 happyReduction_62
happyReduction_62  =  HappyAbsSyn16
		 (Const UNIT
	)

happyReduce_63 = happySpecReduce_1  16 happyReduction_63
happyReduction_63 (HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn16
		 (happy_var_1
	)
happyReduction_63 _  = notHappyAtAll 

happyReduce_64 = happySpecReduce_0  17 happyReduction_64
happyReduction_64  =  HappyAbsSyn17
		 (Nothing
	)

happyReduce_65 = happySpecReduce_2  17 happyReduction_65
happyReduction_65 (HappyAbsSyn19  happy_var_2)
	_
	 =  HappyAbsSyn17
		 (Just happy_var_2
	)
happyReduction_65 _ _  = notHappyAtAll 

happyReduce_66 = happySpecReduce_1  18 happyReduction_66
happyReduction_66 _
	 =  HappyAbsSyn18
		 (TUnit
	)

happyReduce_67 = happySpecReduce_1  18 happyReduction_67
happyReduction_67 _
	 =  HappyAbsSyn18
		 (TInt
	)

happyReduce_68 = happySpecReduce_1  18 happyReduction_68
happyReduction_68 _
	 =  HappyAbsSyn18
		 (TBool
	)

happyReduce_69 = happySpecReduce_1  19 happyReduction_69
happyReduction_69 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn19
		 (TBasic happy_var_1
	)
happyReduction_69 _  = notHappyAtAll 

happyReduce_70 = happySpecReduce_1  19 happyReduction_70
happyReduction_70 _
	 =  HappyAbsSyn19
		 (tunknown
	)

happyReduce_71 = happyReduce 8 19 happyReduction_71
happyReduction_71 (_ `HappyStk`
	(HappyAbsSyn20  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn20  happy_var_5) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn19  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn19
		 (TChannel happy_var_2 happy_var_5 happy_var_7
	) `HappyStk` happyRest

happyReduce_72 = happySpecReduce_3  19 happyReduction_72
happyReduction_72 (HappyAbsSyn19  happy_var_3)
	_
	(HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn19
		 (TProduct happy_var_1 happy_var_3
	)
happyReduction_72 _ _ _  = notHappyAtAll 

happyReduce_73 = happySpecReduce_3  19 happyReduction_73
happyReduction_73 (HappyAbsSyn19  happy_var_3)
	_
	(HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn19
		 (TProduct happy_var_1 happy_var_3
	)
happyReduction_73 _ _ _  = notHappyAtAll 

happyReduce_74 = happySpecReduce_1  20 happyReduction_74
happyReduction_74 (HappyTerminal (Lexer.INT happy_var_1))
	 =  HappyAbsSyn20
		 (case happy_var_1 of
                     0 -> C.singleton Zero
                     1 -> C.singleton One
                     _ -> C.singleton Omega
	)
happyReduction_74 _  = notHappyAtAll 

happyReduce_75 = happySpecReduce_1  20 happyReduction_75
happyReduction_75 _
	 =  HappyAbsSyn20
		 (C.singleton Omega
	)

happyNewToken action sts stk [] =
	action 78 78 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	NEW -> cont 21;
	Lexer.INT happy_dollar_dollar -> cont 22;
	TRUE -> cont 23;
	FALSE -> cont 24;
	LID happy_dollar_dollar -> cont 25;
	UID happy_dollar_dollar -> cont 26;
	LET -> cont 27;
	IN -> cont 28;
	Lexer.FST -> cont 29;
	Lexer.SND -> cont 30;
	CASE -> cont 31;
	OF -> cont 32;
	IF -> cont 33;
	THEN -> cont 34;
	ELSE -> cont 35;
	DEF -> cont 36;
	AS -> cont 37;
	ARROW -> cont 38;
	Lexer.EQ -> cont 39;
	PAR -> cont 40;
	LPAR -> cont 41;
	RPAR -> cont 42;
	Lexer.LT -> cont 43;
	Lexer.GT -> cont 44;
	Lexer.LE -> cont 45;
	Lexer.GE -> cont 46;
	Lexer.NE -> cont 47;
	Lexer.ANDKW -> cont 48;
	Lexer.ORKW -> cont 49;
	Lexer.NOT -> cont 50;
	Lexer.AND -> cont 51;
	Lexer.AND -> cont 52;
	Lexer.OR -> cont 53;
	Lexer.OR -> cont 54;
	Lexer.NOT -> cont 55;
	Lexer.MOD -> cont 56;
	Lexer.TUNIT -> cont 57;
	Lexer.TINT -> cont 58;
	Lexer.TBOOL -> cont 59;
	LBRACK -> cont 60;
	RBRACK -> cont 61;
	LBRACE -> cont 62;
	RBRACE -> cont 63;
	EMARK -> cont 64;
	QMARK -> cont 65;
	COMMA -> cont 66;
	DOT -> cont 67;
	COLON -> cont 68;
	SEMICOLON -> cont 69;
	STAR -> cont 70;
	PLUS -> cont 71;
	MINUS -> cont 72;
	SLASH -> cont 73;
	UNDERSCORE -> cont 74;
	TPAIR -> cont 75;
	TSUM -> cont 76;
	OMEGA -> cont 77;
	_ -> happyError' (tk:tks)
	}

happyError_ 78 tk tks = happyError' tks
happyError_ _ tk tks = happyError' (tk:tks)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Functor HappyIdentity where
    fmap f (HappyIdentity a) = HappyIdentity (f a)

instance Applicative HappyIdentity where
    pure  = return
    (<*>) = ap
instance Monad HappyIdentity where
    return = HappyIdentity
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (return)
happyThen1 m k tks = (>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (return) a
happyError' :: () => [(Token)] -> HappyIdentity a
happyError' = HappyIdentity . throwError

process tks = happyRunIdentity happySomeParser where
  happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


data Pattern = PUnit
             | PAny (Maybe TypeE)
             | PVar String (Maybe TypeE)
             | PAs Pattern String (Maybe TypeE)
             | PPair Pattern Pattern

getBinding :: Pattern -> (String, Maybe TypeE)
getBinding (PVar x topt) = (x, topt)
getBinding _ = ("%val", Nothing)

expand :: UntypedExpression -> Pattern -> UntypedProcess -> UntypedProcess
expand (Id x tx) (PVar y ty) p | (x == y) ∧ (tx == ty) = p
expand e PUnit p = Let "_" (Just tunit) e p
expand e (PAny topt) p = Let "_" topt e p
expand e (PVar x topt) p = Let x topt e p 
expand e (PAs pattern x topt) p = Let x topt e (expand e pattern p)
-- expand e (PPair pattern1 pattern2) p = expand (UnOp Process.FST e) pattern1 $
--                                        expand (UnOp Process.SND e) pattern2 p
expand e (PPair pattern1 pattern2) p = Split e "%1" Nothing "%2" Nothing $
                                       expand (Id "%1" Nothing) pattern1 $
                                       expand (Id "%2" Nothing) pattern2 p

throwError :: [Token] -> a
throwError tokens = error ("Parse error on "++ (show $ head tokens) ++ "\n")
{-# LINE 1 "templates/GenericTemplate.hs" #-}


















-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

























infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is (1), it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action



-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Int ->                    -- token number
         Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction









happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        action (1) (1) tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ( (HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.









{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.

