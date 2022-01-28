{-# LANGUAGE OverloadedLists #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

module Test.Wire.API.Golden.Generated.RemoveCookies_user where

import Data.Misc (PlainTextPassword (PlainTextPassword))
import Wire.API.User.Auth
  ( CookieId (CookieId, cookieIdNum),
    CookieLabel (CookieLabel, cookieLabelText),
    RemoveCookies (..),
  )

testObject_RemoveCookies_user_1 :: RemoveCookies
testObject_RemoveCookies_user_1 =
  RemoveCookies
    { rmCookiesPassword =
        ( PlainTextPassword
            "~\DLE y[b\aS\ETBf\165617x.lY\144244r`\v{\9628\CAN\39987%=\f\1096516U\DC4%\1062824\1060574__'m\RS\DC1\DC1c\58278\47267'eS\62075nST\SOH\38363\&70\184977\16409<\1087023\154326nF\1083847\&2~r$=\1023019IWVMd\23687\v**\CAN.)\128397=Pt..\160303m\152336a\a8<E\1073057\ESC\1099083\22604V7\DC16\24660A\36422\148088\989762\1079451\1074668\28151Hw\143072+\52845h^Wb,08\1022287\25381Xk\1067004~\NUL7!~f7b\132292/8\133506|Xe8\a\92980\ENQq\1043065a\ETX=]\95303\DLEQ\SO!\DC3\169321~\ESCx4\1052602\1016556\&2jy\1112693\DLEwY\14792\&1M\DC1\14549\1081574PsT,OiV\STX1\48533*ub\DC1e\b\DC4$5R\EOT]$c\986512\RS!d \179549)'\r\1051919\ENQgZW%\NAK\n\EOT2\1088214\GSy\DC3\1092091\\\RS\ENQp\39252tU\EM\989157\917771l\STX\GS\DC1dqz\ESC\SO\1046487{r\nxAi]Y&\EOT\ETBgF\a\EM +:\EOT+\ESC\138938\EM2\FS\1073955\67613|6U\1011821(g\GSo\83320%\f\nH\10284\1108310\ETX\SOu\r\1065314&\1100625,,fI,\996101\170211\&2-\136996b{a0\1103636 \1063830%\DLE@\159352gT\33371\&06re\983264B\SO\\\SUB\983119'\EOT8\SYN\1041601cSv\SO\187879\178070\SYN\SYN[J\a\DLEo8\SO%PM\1096182\29349$x\ESC1\190917\1015718KK\1010692Y\28987\&1\160571<0\fX\1028010\176855\1047457\166626|q0\1104225\29030\10493J*\1113017X\1038358\145124\134046XWu;\36917\27079\21119/\1102574tK\1052488\&9\STXn8J@\DC2zz8\DLE\1060654=\67272[oDM\SI%2\133968?CHZN2p\152220S\2824\1048899\&8\ESC\1046577\16969\1022158m~\ENQ\1082023yuw{\1061803\1114079e\tm\37734<\NAK6Xt\DLEu\DC2\rWg|\1024128{vS\1059213&&\48395e@h(pT\EOT\DC4\186944\63794q'Bz2\25378\1005127]\92637!7&N\986698\141826\43492]rH9\1096840u%g\1038214\DEL\1040919\18374\&5\b\f\\?t\DC1\129664%\1099048,\f<\SOH5:8\1013957[D\54190\60071CQw\98657\n\5726#i\CAN\CANJ,{yo\1083169;{\99103hkU\1048743>'\129122z\1026688=e\DC1\1112305\SYNQ/_\100425lV_}(dj\1007316\rZd<\RSB\CAN\1040599\vY\1013052\986793\985671\NUL\ESCJ\1034011tY\21996\69863\FS&s\EM\1112635o\DC2\a7\b^\65317\&6Zi[`o\ACK;\169627\DC1w\18173WxA\1023958c\173780\&3\a\DC2s\133508l\DC4 k=zy\155530\10060\"\37575ex\1058728f\DELQ\1067079\DC2\917961P\26569*\10329\96874F\67677C\DC3\1078547\DEL#\1102527-\t\GS\1113174`\ETBCg5y\SUB\EOT\EM\179479\&1eS\GSRI\ETB\STXN\9021'jj(\1039923\\Cn+zw\fL\"\164893(1\131177\1102357\ESC\US\185088\11429\aoj\98391Y\1019608\"\\\1024267\DC3>9\1009548=\US\6648\GS\153529\ENQ\CAN\1086366\983773\fg\1007968\1061229\149186\&9y$\DC3L8\US\n\DC4\1081485\99847Fh\1021505'\63755&@\36277\138987\1067265\1037682\ETB[\61437A\1068948$D\1021662]\ETX\67726]b\SO\983789C\1113071n\1086865\&9\ESCC=`$\FS\161385g@\160312RS\135404s\59787\&4\1084324\SOH\DC3\1084568\v\1004384#<Q\1022843R\DLE\DC2 \181927%\tcc/4>\144094\166834v\1064183x\1007247)8&y\"\12739\&5\"mV\DC4\1024086A\SYN\SYN\1091794~\27582\SUBp}?n\1058047;\1046488\NAK\1089289\r7\150314?\t\69608c}xV\1014630\49894>\1043598\a@K\ESC42\12076\1001536s\31446~mXY[m\152863\a*l;\1028244!\\|\DLEys\1043026\48317aS-\DC4+\SYN'%\DC44\61424\&5\189792\159439\DC4\1009152\59988ayCtYM\162130L\SOm\69240\71450Mi\177207S\3658"
        ),
      rmCookiesLabels =
        [ CookieLabel {cookieLabelText = "\t"},
          CookieLabel {cookieLabelText = "b"},
          CookieLabel {cookieLabelText = ""},
          CookieLabel {cookieLabelText = ")v"},
          CookieLabel {cookieLabelText = ""}
        ],
      rmCookiesIdents = [CookieId {cookieIdNum = 23}]
    }

testObject_RemoveCookies_user_2 :: RemoveCookies
testObject_RemoveCookies_user_2 =
  RemoveCookies
    { rmCookiesPassword =
        ( PlainTextPassword
            ".]6?|n\a\1065388b`\n\GS\39199*K\DEL\SYN#&\nfu\303\EMo\SOH-\DEL\DC3\161956f\989883g\ETB\60099B\DC4[DG+\NAK$w6]s\GS\ACKNE3\1033233H\131509\&16u`2nt\1019805\&3u!\NUL\46988\1113403\\\149411\172028\EOT\41891DC\172619\38340h1of\USh}e]\51011-VT}\1095536\23412\SOH\1106779\58945\b\59014/\SO\1078889\1016692lsWIV\bvc[\3021g+i{\FSx\1103976\t\30057a\SUB\ETB\1104229\&9\CAN%Ima+\1070890\133992\ESC>c)@6Y\DC4m\b:S\b\1061075[$7\166679\r\EM]\FS`4\8919`caw\DC1\SO\995307\1059173_\120882\60175A<:K\181573Y5\47463R|\CANTzx8\ETX\1108945\186155\96907\USD\1046364~\97956\155949\SYN3\CAN\15406\1094233X\163803;\9600\&2\SYN-+\178365\24668M\153159\&0X\CAN\1075318O\48886\EM\174251&'\ENQ{m@\5450N\1089713$c84\US~#\1051743!\35284C\1053345T\ESCV\145721\&8Fwc\169935J\97503"
        ),
      rmCookiesLabels = [CookieLabel {cookieLabelText = "\26318\33391\EOT:\144276"}],
      rmCookiesIdents = []
    }

testObject_RemoveCookies_user_3 :: RemoveCookies
testObject_RemoveCookies_user_3 =
  RemoveCookies
    { rmCookiesPassword =
        ( PlainTextPassword
            "n\1074004\&8^\1048275\48742x\1063089\EM\70670\1017945a\DC4\1092504<'\DC2'CS5\17737\rc\DC3\1028471a\171752\6694+\SIFwqHI \DC2Cc\156156\1047335n]\995075\CAN\1024943|\155491C\b\EMt\t\CANs\175523\988484\f\1045889\179665-X\ETXh\1010180\1038017'O\1004140i\63367\ACKM]\162300\&1T3z(Sb\996128\986764\1009876\&3\1019290\988277\1026196LM\STX\SYN\31631?\r\1012626@/R7;M\NUL1\r\1110659\CANX\1100936\EMQc\1102268f5#k\NUL\ESC\153067\SYNE=9\SI\DC2uw\SUB\DC1\EOT\1054510{\63090\SYNi\92523a+\DLEZO\"W:Wk\6376wg_J\US+S~E&1\165458\1034011\27203?'\157835\119845%TY\998234D({.\9336\&7\133572F\1022194&g=\1051853|\1072901A\a\DC4\CAN;P\1024587S\SOH1GyP\18999|\1048580s\135528G\9609IGB08B<\1097349\1063644\CAND\ENQ\992040|~y\DC2\v\984222&\182974\SI!z\DC2 \27161M\29167\EMW5\vN?]!v\172138,1\182336\STX%%\EOTv\SYNg/\144764\1081383\32652\1079881,3'\7545)\DEL\ACKP\CANI\US?\DLE\126073]\139395\1087857bo\f\1109978h\1044925i\SUBxI41Sf\144057\182522\153605\v\US\1024502\v.(\GS\EOT\175982$\DEL\58992\USEQ\177834!!K\1047971Q\ESC\145189`;\1092648.\ETB\ESC\FSRN:\SYN7\ACK:\n\154169\1023167<\146858:\993302E\b1JN\1017985\ENQWAK\ENQdAXD4[O\\>\RSxV6$\v"
        ),
      rmCookiesLabels =
        [ CookieLabel {cookieLabelText = "E9"},
          CookieLabel {cookieLabelText = "\ESC"},
          CookieLabel {cookieLabelText = "\134960"},
          CookieLabel {cookieLabelText = "\SOT"}
        ],
      rmCookiesIdents =
        [ CookieId {cookieIdNum = 1},
          CookieId {cookieIdNum = 1},
          CookieId {cookieIdNum = 1},
          CookieId {cookieIdNum = 1},
          CookieId {cookieIdNum = 0},
          CookieId {cookieIdNum = 0}
        ]
    }

testObject_RemoveCookies_user_4 :: RemoveCookies
testObject_RemoveCookies_user_4 =
  RemoveCookies
    { rmCookiesPassword =
        ( PlainTextPassword
            "4\1102804\1064901_\GS\STX\18346\&5J++\144782/\nR=8m[\21769S8\154932|\168718\&6\1024454w\52721\161903nTK\991546E'[l\34792$\47524\45942\1026587\1038545\DC49w\FS+\189755H\DC1n\989334\ETB\SIJ\ENQ(M\136816\SI\v\f:\NAKr\151754\1046700O\ENQ\63854\46485\1005290*\132235\1043453\154333Z\n\147930j\995537!b\66478\21782K\b\164738c\83125\v{Zm\126559)\DC4\1111162\96336\1011262\SI}M\1025962_\53279}&\989788\DC41\DC3\NULr\1052010r\119595D/H,\SO\SOH\SI\1038741\nD\54315\&4H7LK\1008789DM+!\GSY\f%vof\1007306\NAK\145073\1060272\139970\62576\ETB\SI\r"
        ),
      rmCookiesLabels =
        [ CookieLabel {cookieLabelText = ""},
          CookieLabel {cookieLabelText = ""},
          CookieLabel {cookieLabelText = "I"},
          CookieLabel {cookieLabelText = ""},
          CookieLabel {cookieLabelText = ""},
          CookieLabel {cookieLabelText = "Y"},
          CookieLabel {cookieLabelText = ""},
          CookieLabel {cookieLabelText = "}"},
          CookieLabel {cookieLabelText = ""}
        ],
      rmCookiesIdents = []
    }

testObject_RemoveCookies_user_5 :: RemoveCookies
testObject_RemoveCookies_user_5 =
  RemoveCookies
    { rmCookiesPassword =
        ( PlainTextPassword
            "&\8450d\2317E\1071031\162311\DC4OR\SYNE/4\SUB\n\bI\USz\ETB\1037079~lI\170695Id\SUB\72819G\a?\1078248L1\172461;D\ETBsI3\DC4\GS\1111322p\SUB!E2\fF/K\nE:R\DLE\n\185553\174465\EOT\1008445f9"
        ),
      rmCookiesLabels = [],
      rmCookiesIdents =
        [ CookieId {cookieIdNum = 1},
          CookieId {cookieIdNum = 0},
          CookieId {cookieIdNum = 0},
          CookieId {cookieIdNum = 1},
          CookieId {cookieIdNum = 1},
          CookieId {cookieIdNum = 0},
          CookieId {cookieIdNum = 0}
        ]
    }

testObject_RemoveCookies_user_6 :: RemoveCookies
testObject_RemoveCookies_user_6 =
  RemoveCookies
    { rmCookiesPassword =
        ( PlainTextPassword
            "w]\159418_H\172755\137808O\67751p&\1057267z\EMl)*\US,'\4481\STXz929v\42411~\a\15834N\STX6\STX\GS <\DC2\no~\1027753Nn\166014\1002433\1037607jH\ACKN\ENQIL598\NAKw\47985=Q\1075460#\1012931k\GSb$%\96700\163122\40349\DC2k\b\63286\FS#\"\40898?\1026494sE-W/\\|\SO\SYNjzYW\22937s\1101861\21205;'\1038086,}c2\1045179\NUL\ETX6\1036582\DC1wNis\DC1}Q\1074930a\1030918qd\996927JOUK\78101$a\SOH\75036\1091437\1109347fY?\SOkX\70170\191402\FS5\38440\1100416i\US(\ENQ\83248\52600}\DELm\53151=X\DC1\ETXM[6[\169507x<Q\67688}\\V<\v\1045716\111287q(0\DC3\SOXYB\171786QYyJW0r3\US?\132524:\992443/|`\ACKb#\DC3l\999631|=xK0:\SYNc\rM>W\15060\989837\b\SI,\1076623\49985\RS\RS\SOk)\44862zE\47080\ESC\2315\1090173^\1060289]\20780\EOT\97838\135916\a\138562CA&\ETX3YK'S2+fI&\1092619z\24349A\24648\&5Qu\187043r\1097016I\53422Z\SOHdv\1113858k3\GS\DELN\110964\14426\178818\54900\SO@\1020841\1073781\&5\STX\DC2-\STX\1059065\&7\r`\EOT%\SUBN\9521!3\ENQ;Pr\134932\11590\144594(\1059588\STX\1092686\GS\22445m[\SOH`\GSB6\1093918C[v\19595Sq\NAK7\DC1\45766\31039&\150919\48875\63709JTBR\STXp\180507?y\15242A\58153ik\1100370s:Yl\138494=\37874)\rGU\65530\177230\157036h\1109341\988577\9048\&8Q\a~\18382\SUB\1094063-}1v\18881h)\1052487~G\48401\"I1?\154186\11104%Px\DEL<I\EOT\159233\1022740\23564z\EOT\1085875BfOC\ACKit\ESCn\"%\\\1069004I\97502\168427\1003124Q\ETB\SOH\v\63032$M\1076234\ETBC\DEL\RS\EOTW\1088187\1005096\1055438\tD\1030132]\1009400W1\121312e\994020\998842J\DC4t\97745\SO&\SI\DC3\46643\1108114d>th\DC3su=/\9183\143001m+\CAN\7444Z\DC2\133207\RSXe\DC3[\\8>~\SUBR\NULY?0\SOH?HI\be\1063343\&5\STX|\ETX,, e\1009518\t\SYN`-\20161\DC2\EM\1069697=\NAK\NULDrL54\CAN2Fm\1019712\&3\DLE\41731\19699\NAK;Jv\SOHb\47652\1077833(9\1053955DV\1094400\STX\47484\37476\61515L\bS\STXQ\NUL_\1066043Z\987480\27777(\SYN7\SOHT\1053283\131547PhI\154346;\171904[J\1033568POxH5\SOH\fu\1083780:\1087406\fx\"\160644\DC1\ETX\53105d\160880}\1047755>\ACK\SYN=j\ETB\1000264\22614mF\1040449\&6\tl`\357$\7983\1046005F91\1092328O\SUB:L\1030242\1044272j\1056871\26141SQwM:>\1073831|e^ixFd\a{\t\155098\f6q)>>&J\USm\DLE\63897A\SYN\ESC\145354\4649dP<i+K:kd,$`\1055080\DC3\DC4^\148885\&76C\1057344g\RSwE\1076448\63843zDu=*\DC2pN\1089997\n,\SUB\122883\1068397\17997*\DC1{\30794`rf\1106403o\1036653\SYNx\1094094g<Iq\29538I5(X9D\1038674\66600&l4\149809\NAK\"\152016\&0\1042785\EOT\v~%\SI\100794d\f\EOT;eVyM\DC4\188195\1046008\&7\1110059.\1107644\1011094\NUL\163296\1065513+c\FS\GSQK^\ETXo\23679\15202\&7\DLE~f\1004637\EOT\19456y\US\1024565t#Mpq\748P\t"
        ),
      rmCookiesLabels =
        [ CookieLabel {cookieLabelText = ""},
          CookieLabel {cookieLabelText = "*"},
          CookieLabel {cookieLabelText = "|"},
          CookieLabel {cookieLabelText = "\190744"},
          CookieLabel {cookieLabelText = "K"}
        ],
      rmCookiesIdents = []
    }

testObject_RemoveCookies_user_7 :: RemoveCookies
testObject_RemoveCookies_user_7 =
  RemoveCookies
    { rmCookiesPassword = (PlainTextPassword "\180707H+[j\1071861\DC4\CAN"),
      rmCookiesLabels = [CookieLabel {cookieLabelText = "s\n\128474gM\DC1\72804l "}],
      rmCookiesIdents =
        [ CookieId {cookieIdNum = 0},
          CookieId {cookieIdNum = 0},
          CookieId {cookieIdNum = 1},
          CookieId {cookieIdNum = 0},
          CookieId {cookieIdNum = 1},
          CookieId {cookieIdNum = 0},
          CookieId {cookieIdNum = 1}
        ]
    }

testObject_RemoveCookies_user_8 :: RemoveCookies
testObject_RemoveCookies_user_8 =
  RemoveCookies
    { rmCookiesPassword =
        ( PlainTextPassword
            "\GS*\1051754=!ns8L({9V.\a\NAKX\n9AH\NUL\10750mH-Ys\166881zI\1025099\n- QHAP4\42401\DC31\15173\52485`_.\v-g\168203\v\DLEk\1100665jcu`\153701\1110229m]t\rrAggj\GS~Ms\ETX\33009^.:B\ESCUv\ACKzE:7"
        ),
      rmCookiesLabels =
        [ CookieLabel {cookieLabelText = "z"},
          CookieLabel {cookieLabelText = "\t"},
          CookieLabel {cookieLabelText = ""},
          CookieLabel {cookieLabelText = ";"},
          CookieLabel {cookieLabelText = "0"},
          CookieLabel {cookieLabelText = ""}
        ],
      rmCookiesIdents = [CookieId {cookieIdNum = 2}, CookieId {cookieIdNum = 0}]
    }

testObject_RemoveCookies_user_9 :: RemoveCookies
testObject_RemoveCookies_user_9 =
  RemoveCookies
    { rmCookiesPassword =
        ( PlainTextPassword
            "\20276J\7142\&7\1061264\&9\1055944.\6672]0\ENQY^_'\65607+\1005219w((\1055660e$37!$, t\ESC\1015270\&0\1000323\989086M\41529B-W\154921M\rc2\1063448\\\181251\1105565r\1012740mG]}21M1\994303\1098837#\63934\\\28337\&2d{t?\DLEF\1056242\tR\GS\NUL\1060167\DC3\ESCa\EOT\1013823\128012\v]\1063336n\"V\a0\159763\1091504R&0\1107029d3\26546\155458ec\1006455\n\1054839\SO\1066809Yk\DC472\FS8\140158v\987214\94371s\DC4\STXrQk2Ma*=c\1016534\992775*2'\1030247B\26647|b\FSD\14666k\1068747i\188999\DELy\ah~\34082\169294\ETB\ACKE\\Rsc.D\b\\W\1011731\1002696U}\158890slC\DC4%\41408\990190L8uZ\1054052E}\nP(\DC4<\tf\128453\1062122\54976\54714!\ETB\1000764\1009846\&3\DC3qa\147977\1052097L\SOH\1063492AL\ETB\r\8666\GS/L\21479G\DLEw\20328\155133\DC3\f+b\183575k\1051820\1057050l#I\DC2\DC30\1066598Z\1054486~\RSh\1110805\ENQ\1031472\DC3`\72840\1012108\EOT\1001074\1025854\1105585ud,\1101772^vx\CAN\ETBC;F\a7#&+#k(1\b\176318\170363\STX~$K0!z\54246\1009299\ETX%\1055859\1101826\ACK\DC4C3_\29624\ACKh =8|$\157738,f\39391\19853\1088057\a\US\148781\SI\1064830$(u\EM0\r\160844a3\SOMV\DC2\184024v\1061595/\DC1\v\1109084\SYN^:\t\rV\41865~/]\ESCqK\1057017%\63883\DLE\23673&U\SI\47423G\DEL\US\SUB\176746\DLEaJ\96500\ENQ/@\ACK\1079779i\SUB5\1050326oz\US?\992390c\1069199rZ2\147008 \EOT\181871\rxseov\1018185\SI\164602)_\SUBa\1027176C\139112I\ETX\ETX\93028W\983732\SUB'L\NUL'\17068\1064062\"2\150172'#<TwqJ<\1064227\EMs\1062041ctId\137789'\1073122\RS\998297\78457SB\NUL\23335i\167621\992313\EOT\153684.5\1075945-$:Y\SYNH'W\13859\bMx\132059x6m\1030248\DLEdSpSGn*\EM9z"
        ),
      rmCookiesLabels =
        [ CookieLabel {cookieLabelText = ""},
          CookieLabel {cookieLabelText = ""},
          CookieLabel {cookieLabelText = ""},
          CookieLabel {cookieLabelText = ""},
          CookieLabel {cookieLabelText = ""},
          CookieLabel {cookieLabelText = ""},
          CookieLabel {cookieLabelText = ""},
          CookieLabel {cookieLabelText = ""},
          CookieLabel {cookieLabelText = ""},
          CookieLabel {cookieLabelText = ""},
          CookieLabel {cookieLabelText = ""},
          CookieLabel {cookieLabelText = ""},
          CookieLabel {cookieLabelText = ""},
          CookieLabel {cookieLabelText = ""},
          CookieLabel {cookieLabelText = ""},
          CookieLabel {cookieLabelText = ""},
          CookieLabel {cookieLabelText = ""},
          CookieLabel {cookieLabelText = ""},
          CookieLabel {cookieLabelText = ""}
        ],
      rmCookiesIdents = [CookieId {cookieIdNum = 1}]
    }

testObject_RemoveCookies_user_10 :: RemoveCookies
testObject_RemoveCookies_user_10 =
  RemoveCookies
    { rmCookiesPassword =
        ( PlainTextPassword
            "\ENQzM]\rS\1055321\b0{\STX@@\SO(f+\1070907^gA\STX*_:3\r8f\138443AU\71935\DC4vxT\STX\f\DC4\1073392|\n\1108606z?\9075d|x\998428;_.E\FS\\y\1022175\188385\&1%W#qrI$Q^7\NAK\48016\n4YLf\GS,BJ\"\2026\1034719]Mg@\177727\10438!}}\SOC4\EOT;'&\132440\RSZ0z\66713#\ENQ!km\1019861\166421\RS@4\100641\SYNam\59473\994022Qe\62357Jx\US-\59179C\DC1\EOT\1082162\&7\1014872\DC24%\r\1075426\SUB\STX7\ESC\DC4&"
        ),
      rmCookiesLabels =
        [CookieLabel {cookieLabelText = ""}, CookieLabel {cookieLabelText = "mcQ"}, CookieLabel {cookieLabelText = "P"}],
      rmCookiesIdents = [CookieId {cookieIdNum = 29}]
    }

testObject_RemoveCookies_user_11 :: RemoveCookies
testObject_RemoveCookies_user_11 =
  RemoveCookies
    { rmCookiesPassword =
        ( PlainTextPassword
            "\SI\143351[\EOT/E\1080398m\NAK\NAK4[\1000835Jc\ACK\35519\&0\t\DC10}\v\CAN8U\26011\1036154Hi\DC2J_^S\52693\1092158n tv)\1097408P\n1)\1076605\r\SOH@.\1072145\CAN-\f\CAN*O!\SUB\fI;BK4ko\DC2\b\1016801&x840\136302\68352+}!\ESCn9dLO}\SUB\b\44670\&1\148402\SO2f\1009018\1031411\174771\"aQy%ii}9w\\\158219*\94294SH?A3\1110635\ENQ.\RS\1051911\&1`\1006043*\983847\67707\133301i\DEL\ETBkM\SUB%;\1110917\FS\989883\36467\1021399\SIv\120356\v\153819Y\DC3\ETB0'=\1017993g\996624si+sgr\SO\1066910G:\r\1066359`g:\ETB\189311?\189037\DC24u\ETX)\99946\4243nW\US.\DC2j\1034775a\179199\18264'Q\f&#K1\SOP4\64553 []ce\b9\1076672\FS1\r\1009284\SI\ENQ}/`9 jr\SUB\t?\194634Q\155956Q\SO\1097613o\1107004Y\167030LRS\STX\DEL*\171130nv.QZz\a~L\1053637\&93\GS9\1083085\raoz7\bL 5ckd|E\SUB~}\b\STX\1030123\STXC7\SOH@jHFw^\48173\CAN\STX\1108317\SO\ETB][u\121448;^\36335\&1p[\1001884\n\ETB\1113524n5h\CAN\1011396.\SI\165672\ETBN6j\STXTZO=Ui\148851\NULG`M>HIBp\US\DC3\1064840\EM+x\NUL\nk)\ETBS#wzkV@\1047484\162584`\1108477\EOTf~|M\1064576ju\128005dz\161287(\154242\1030107\169500\171140\993366\DC46\EOT{c\EM\DC1e\42116NE\138454\&1\44451\a<&o\61411\168863%Zi:Q\173515\US\15783:\DC3)\1077093$ViLW\SUB\"m.s3\179500e\7133* \SOHyny\EOT\DC2 xfa|d8@I\ETX@D\149538\ETB}w\f\1076872\ESC\ESC\156414:\22792\DC4\1009280fm\SUB\46688g\155275#n\158905\"\ETB8Z\FS*\154535!\1042094\US\24564\1009578>r\174073\DC3(\47427\EM.\rc!\RS\SO\DC4Mve\ENQ\155638&\"\1038446[k<mlCI@\EOT\ETX\1086089r\1073829*;_\35130\50496\ETBXo\147474\1061511G\"?<\ACK\161730\STX\r\"+N3\DC4y\ACKv{\ETB^\DC1fb>\1078916\ACKw0V1\1080263\153959\1070816\60593\37795\988596xt\USXI$\1040297Zgw\190774M\DLE\GS\144429\1080305(\nFot\n/\DC3\191281n\DC1/\r\1094779%1\STX/Yw6=\ENQ\991859>\95544\DLEO\1088332Q\124970\9347\&3O\SIo\176451hmVar\NAK\EOTCw\DC2\STXqKCE\EOT\1014559=~\FS|=#\191167\34136\1076113/_#a\22856N\FS\132958\DELt\1058130\1055453d<z\ESCdj\\s\1002671\1038643\DEL4M.\RS\EM\1027681\SI\161-2.\"7cxY&!Vc\1091729\1029383+:\1101036\SOCm\"'\af\1013663Iv(\162409*j\18218\a\US\ENQ \f\187855\1024784Bq~\998286eeo\132650"
        ),
      rmCookiesLabels = [CookieLabel {cookieLabelText = "a"}, CookieLabel {cookieLabelText = "g\STX"}],
      rmCookiesIdents = [CookieId {cookieIdNum = 0}, CookieId {cookieIdNum = 4}]
    }

testObject_RemoveCookies_user_12 :: RemoveCookies
testObject_RemoveCookies_user_12 =
  RemoveCookies
    { rmCookiesPassword =
        ( PlainTextPassword
            "\988760\152429\&5<S\\\"JI\28049K4k\ACK:Z!\SUB\9206mq\41807e[\1069840\142761\19477\aDg\160957N!\92497\189746\SOH\RSX\1005835W\987840&\GS5\rm{\53807\STX\DLE\39183+$g\25132\DC4\32645\45092B\1040089}i3\179276,x\DC2f\a\f\DELx\\MJ6s\131591N\SYN4&~\74413\1033800\1071378\&6h\55055LM\CAN\1018991tx\t\RShP\1003269<\2878L\\kh.\917604ufO9\RS|\EOT\DC2Ec\NAK\1013546Q\ETB7ag'\nX\1037749\1071030\ni\1107523\138522\57960\63246\&0\184088'W\12959&\DC3\NUL\SUB|\US\v;\1094492W0n\1011634\1018055\DLE,0\65456\990673f\SIA$\24562g\178159\1062439\33686/\1026612XW\48207\1004137]XeN\175810\SI;\7659|+\1004879\1040103\bz1\1084373\988908\v\1023806k\17578~\GS\65398\DC4>,C=b\1072988\&1]x~\NAK\1030595\1043441b\58981\1003992\NAKn\1073851:\176269\1047965\24337kk\US\25317\31713\137045\45961\US+\33078\990516J\6312Z\t\68213@\1096088\1012809\&4I1:\NAK\b\160700\DC2\t\n\DLE\ESCp\DC3\161175k\26439f[Uq\SI`\1026102\1072178\188473\DC1x^\SYN[6nl%\1031781L\USX5^H&O\GS\SUBE\SO\10205\1068465F4\167670$\ETXIt,U\f4\ETX\SI\176494\ENQaga&8I\82998X%\1073997+`\93020f\987218A\119129OA|Lg\STXX\FSa%t&!\1023870\182892>\DC3\EOTlX+\DC1#\STXz\1043705t.|\EM\f\ETB\39680nr>D~F,\r\147801\DC2'Xv\"Y9w(ewO\CAN\168923\&3O \STXt8cK\SIW\16221\DLE\1020099\ETB;~!\ns\1060956F/p\NAK#\29723\ETXS\986769_r\DC3rP\SIa\47748X5-\996337wi\DEL(\41245\1111170\DC1m=\EM\ETXNGf\SID'\NAKq\1017502\r{\37868\1067045\DC2o\989345bZ\NAK\GS\1059619\EM0\187569\DC2\172967\NUL\nkh\1044166vsZX}\1024791.1%3Q\168727%\v\1110210(tmJ@\"X\"\32752c\EM6\995028r^\18683\STX\33207\NAK\150662\142232\&1f_\DLEC\993591c$\60318\aK\1062209\&0\DC3\1010730/H&\176383\b\160276-\r\1006544\&8A/}\US\66272\EOT\33298)\169967\SI\1022721\20001N\ESC\995853\46054{\a\125012\SO[:f\148243\1000397\59885\180729\EM\1017127\40168$\83501\1024153\1106750e*G\189328:wq9\31833W\1078\1060331\FS \1039837\97120\52002\137625\23159r\a Rap|\24194^'\DEL\1030264\ENQ\vv-=)\DC3TH\41337\SOH\CANC\ENQ\FSDi5\ACK\n\fy\\\18236zP\SOPx\DLEc\1000266\25869\1047578\1010027\1040769\DLE_%\SYNJ\ba\53088\EOT\1049972ZP\1034956\32225`\15608\990258\SI\SOHA \DC4wE\ENQ\STX\100145/\SOH\NUL9E\FS!\1076691q|\DC3\DC1$b\159600p\1001300@\RS|m\ACK\986847\94392\b\1057250[Ld\92689\&1T\27296X"
        ),
      rmCookiesLabels = [CookieLabel {cookieLabelText = "7\1007781"}],
      rmCookiesIdents =
        [ CookieId {cookieIdNum = 1},
          CookieId {cookieIdNum = 0},
          CookieId {cookieIdNum = 1},
          CookieId {cookieIdNum = 0},
          CookieId {cookieIdNum = 1},
          CookieId {cookieIdNum = 0}
        ]
    }

testObject_RemoveCookies_user_13 :: RemoveCookies
testObject_RemoveCookies_user_13 =
  RemoveCookies
    { rmCookiesPassword =
        ( PlainTextPassword
            "r\a\SYN'o\62168V\190230mk\96544\&4\ACKiE\1002439\n]\SYN///\54880}c\NAK9\RS\6820U\vOc\EM\167131\b\ESC \119910<q\100710\1023906\a\171413\SIQV&HA\ACK=\ENQ\146799\52977\31795x^\144681Ce\GS\SOHpOEl1\US&l\138944\1059742W1\ACK\1047609\&8\175332\ETBk{\EM\92545\4357zL\toO(%\1072353feWH\DC4\1002681j$a%F\16123Ld\158090\NUL-\988716:i\47995\b\td\ESCPL\1072052H\NUL\DLEiA.s<\173591-fL\1045793\n$+#\1002272\fDGWS\SUB\SO\SOpR\15529d,q}\SO\DC4\988324py};Jy\DC2\121288\STXJ.P\1031234(Se!!\159864M\17997\RS\nP1\1085673@P=<Fc7s\15477\190167;\12534DL$ \NULD_,i\SYN\1055283\fg\STX[\\{\1005453 \US7\1037386\1009872\1051391\&0\150557V\25361\&2\151126d4\n6z_9iQ\189326R\NAK\SOHby\8677L\1021605.R\1059649\&4T( A*\31745HSS\DEL.w\157864\&5\US\1094605\SO+\f\CAN\1113087>-\1024682\SUBwN\988237\&79\1009173\7678\SOH\1104366\176682\DELaD\ENQ\1077188H\176394(Z\a\ESCK\63389@\151539zfD9{Yu\DC2/EC\CANQ\ESC,5\1100500k@-\1034270\1018333<\168865\SOH\DC1J!\917791=\189915\1031587\180630J=N\155206\98572\fT\164918\65531Zd\STXp\SOHju\DC2,\990766@<n\DLE\1038\63187`\134517mf\1075395~m.G\ACKn:\1009609\DC2w DJ\92303>\ETX|\ETB\ETXX!\DC1uZ.\1041808\1016592\DEL\SOl\43846\1077733\&4!Z&_#m\1081763|L=\3360\SUB[\r\1025059{CN\998229\171237\&2\1015391\afl\144022\1068127\60998\&73k5\62642\1084159\45746\1098058\989650\150649\fLmE>V\GS@K\1035300\&3,e!\DLEcV\ETXI\GSP\ACK\b\ETBb\CAN\SI>"
        ),
      rmCookiesLabels =
        [ CookieLabel {cookieLabelText = ""},
          CookieLabel {cookieLabelText = ""},
          CookieLabel {cookieLabelText = ""},
          CookieLabel {cookieLabelText = ""},
          CookieLabel {cookieLabelText = ""},
          CookieLabel {cookieLabelText = ""},
          CookieLabel {cookieLabelText = ""},
          CookieLabel {cookieLabelText = ""},
          CookieLabel {cookieLabelText = ""},
          CookieLabel {cookieLabelText = ""},
          CookieLabel {cookieLabelText = ""},
          CookieLabel {cookieLabelText = ""},
          CookieLabel {cookieLabelText = ""}
        ],
      rmCookiesIdents =
        [CookieId {cookieIdNum = 2}, CookieId {cookieIdNum = 1}, CookieId {cookieIdNum = 2}, CookieId {cookieIdNum = 1}]
    }

testObject_RemoveCookies_user_14 :: RemoveCookies
testObject_RemoveCookies_user_14 =
  RemoveCookies
    { rmCookiesPassword =
        ( PlainTextPassword
            "\144709\1041442 q\78098^$8u\127324\SO\1044337\188234]G\6701aN\GS@X5jWZnJ\DLEy\1073950\174462\DC4if8T\1023255B\1097694\1029784z\33299\77922\161466CQ\1112827*\ESC\997114\&4L\DC4Dl\RS\STX\1033395 n5\1053371\1055345\ac*/\SOHn\CAN{K\1030765\162633A'%Hm\194943\a7\1023991\999654r@YSW5I\RS\137767\DC2J>ZC6\SOCn+\984949mdiK\FSS\165809$\159400\154650;g\CAN/(YfSO\FS`h\166140\1029155\ETB\DEL&TMoO\EM}PJ\ETXJ\SI\NAKwE\161621\NAK\ACKR\178897r3\DC3i\ACKs^RN\1110843o6E\NAKa\n\1069674\100758w$C\1066228\100483\1096666\98103\1044981{Ii\25764W{\t@\1108548?$\57746Y`JA\\V\DC2m\1094766\1093862\&62N\SIC\rK\183210\8084\60608\149539\1069467\1095302\182768\tGO\73813\a\1086319\SOH;\1088275^3\rY\17399i\NUL\2641S2k7\RSO\1035449\DC3\1025441[O!\990640\ACKN\RS2\NAK\7590[\DLE/\ETB{?,RNNM\DC2\SOH#J\vbA\34763\46139\1014229/\ETBf\US\110597\ACK\ai7N\987283\a41\SOH\156359\DC2~:f\SI\NUL(\172432\ESCCo1N\nx4M\43037\ETXUjM^\1052130#\1086102\ESCbL~=HM6Ud9|\SO=\1011725@`\r\1015961\ESC\">i\1025923\&4\101053'\SOwM\\\187240Olq\SUBo\RSZ\13073HB~\"s\144744\\gO\26365\&8\1102131.$\SUB\1055833r\52838\151195\CAN\DELdH\146689q~\1059688e\DEL\47543\26876\&9ZwHf\61877\1056592\1092993m1\vb\NULC#\NUL\SUB\ETXZ^\986446ig\1008052\US_\DELv!Zn.!\34745b_\190192\DC1\62443\159822\NULu\ba\170683)\13139o2\ACK\1043964\1038159[k\53126i\1106233\fu\1055760\152727>y\1057898}\1105150\1067962S/\SOH"
        ),
      rmCookiesLabels =
        [CookieLabel {cookieLabelText = "\"\1017491\a"}, CookieLabel {cookieLabelText = "WG\1030572\62089{"}],
      rmCookiesIdents = [CookieId {cookieIdNum = 1}, CookieId {cookieIdNum = 2}]
    }

testObject_RemoveCookies_user_15 :: RemoveCookies
testObject_RemoveCookies_user_15 =
  RemoveCookies
    { rmCookiesPassword =
        ( PlainTextPassword
            "8\1096227]\1005032*ZB\1070453\128871Z\166100,Pt\"wf\1090677\1047788l\NUL)\442\1013744\1090379\1081560\1034157%f\171139WeL\DC2*\SO\35933\&0S@:\DELi#d\aN\50148dQ;\\w\NUL>fR\DC1PzIM\v<$`'8\165371_P\987002\1081070ex\1082394M\DEL\29721\SUB&-\1081097\1072025e\EM0\1018606\52274\984683\DC2\US\t\1034976\1056657\1056102\1010831\DC3\DC1\23452\r^pZ]>zFu\78656\"L\NULl\GS\169309%;k6/<\194614\1068607\1067112\1104518Q^P\162756\1018086D\137278\47522Q;\1095424\EM;\SUBk\96065&\EM\CANj\EMd9\1063514\&4|~\140375\ENQ\1084306:}\1070899'\178741\r!\1058004\DC4G\149855SUi=1\29733\95046O\EM\1103130fxn\EOTO\ETB\f\f0D\EOT\1047024\SUB*Y (\NULh\1066131\\\1096957k}\v@3N\160900\&0\1103512\DC4Xyo\119325\1019179\&9)>n\1053969s\STX3l\171104\SOH\f\RS\"\FS;\SOH2e\30047ioG]\185478\38451\159767\&5IHH?\SIO[~/\DLE+\nH.\1091753F>?7\1000332\1020204\1095293yvBM\986136J,F\68335u\31826\158325\34549I\ETB\DC4Z/\1091168`\144049:\r\fD\ACK\bqz\9653\54822\\\STXa?\1007578|\GS>=M\v\169342\1112091\163283Qi\ESC\ESC<,uT\30310Y\STX\30565\&2DC\1060622\t\DC2x\1091176:S@|1<nq\ETXQ5D~u\1070352bn\142517\1109146\NUL\CANv\29263\&8\157039\54039\ETB[s\RS1\166498.'u\DLEb\37282\"#\47732\NAKRf\1073297!\ETX\1025007BL\119189mX+\1088821\NULE\f\RS@"
        ),
      rmCookiesLabels = [CookieLabel {cookieLabelText = "\1044053|\121034\94104\NULv"}],
      rmCookiesIdents = [CookieId {cookieIdNum = 16}]
    }

testObject_RemoveCookies_user_16 :: RemoveCookies
testObject_RemoveCookies_user_16 =
  RemoveCookies
    { rmCookiesPassword =
        ( PlainTextPassword
            "\35406Kxx\td\40683\&2\144148;\RS\188326\ACK\fp%\f]T\r\58116I\DEL:\6622m\57763\7765\1025341W4{\DC2\173989\nY\GS|\DLESP\fWO\170628\&6(\GS#\SYN\1024292\&8v\NUL\64417\&3\164217\GS5\1005806~n\1068126AF_m\EMi\NAK2\66617\v\47166|7@\n2~N\b\63934\USQX\1111181vyr\CAN\ETX\ENQOk\EOT\v\33555\RS@w\58041\DLE\ETXC \EOT\DEL\41288&\\+\1004282\61295\NAK\1084576!g\n[%{>h\US\1089192\EOTh*B-\DC2\v\1091993@T\ACK\1013085pj\9481\&3i!\96510M|\DC1\FSAID\EMeXtMzq\SIn\1001835EBs@\b\FSi\RSrgD\144430+\45871\CANi\1000497&\139423t\23767\983347\2668\DEL\1068924<3\EM.+fd\39907B2\1029066\FS\STX\180134taa\1024831G\ETB\n\EM\151241\32939\1110848UV\65353$\984465\1058295\aaN@\152052pL\3978\NUL\1049575\rT/\ACK\1053411L\NUL&G\r\134346\&7-dB\ENQQ\143341qQ\ACKY\1032329\vP/\b\95910q\EOTK\2866\ETXR\RS\94730\1111649\ESCR\SO[\f:\GS|@\"\190867\119249\1073888\1037623ma\t\48187\DLE`\r\1070282\FS!M\v\126482\183394y\184184D3=A\1112567\&4sZ8,&a\DC40\EM\188383\EM\159443\1000258\DC2\35527j\FS\1011085\EOT0]:\USu\CAN#r\STXE+Ov\44692gb\1016640\CANZ[\1089017]\1025831\157479n\1066201X!\1071565\1001761\174983>!6 q\4912\1043252Bf-y`\191450f;*\110614B\28419\SUB\188203zQG\1019466\12802d\ETBkGj\"\1092749\21771\30425\NAKT\59321\STX\1003641S\v\1023077\&8P|F<m!\STXF\156109\NULqj-[X57f\4337\142390\&2G.S\1016624@\ESC\NULm>\39285C\SOHs+#\100532\120405\ENQ\DELtT\bY\a\993728q\1066350?\1071701lm^\1024461ijL\1057142\1028607+\"\ETB\176470H\NAK.jZ\60417B\119156I\\\GS"
        ),
      rmCookiesLabels =
        [ CookieLabel {cookieLabelText = "\DC1"},
          CookieLabel {cookieLabelText = "r{"},
          CookieLabel {cookieLabelText = "?X"}
        ],
      rmCookiesIdents = [CookieId {cookieIdNum = 0}, CookieId {cookieIdNum = 1}, CookieId {cookieIdNum = 0}]
    }

testObject_RemoveCookies_user_17 :: RemoveCookies
testObject_RemoveCookies_user_17 =
  RemoveCookies
    { rmCookiesPassword =
        ( PlainTextPassword
            " 6*\NUL\53342r\DC1\165656}>48\DLE\1083939a\ESCu^\DC2r\1096635koq\1104892@\17218f\1029877:\1069113g\164097Pr'\22073\177171)\GSDE/`\DC3\SOH^M\SO\1041660o\DELN$%x\54111\125060\174761i\169089W"
        ),
      rmCookiesLabels =
        [CookieLabel {cookieLabelText = "\21985\145626"}, CookieLabel {cookieLabelText = "\172449i\GS\993013"}],
      rmCookiesIdents = []
    }

testObject_RemoveCookies_user_18 :: RemoveCookies
testObject_RemoveCookies_user_18 =
  RemoveCookies
    { rmCookiesPassword =
        ( PlainTextPassword
            "\DC1(\189756\173754au(_i\999804kz\1087491K\DLEgc\SUBP\NAK\1087664\&4\162570]\125004\SUBZ3mS\175582A&\1030993\1089970O.=\EM,\170655\35143\US)\r\t%b]\GS\144263ne0\ESC\165573flCO\24483hI\b\ACKqM\38155JL\SYN\DELD iP;9.ms9\ESC\\F\NUL\EOT8\148621\1084732]g\r\59002\994545\&8\SOT\DC3\1108348~\38502\ACKW>o`\RS\42469\&9I`Vm,YFt@yl\1089376\166159\&7_\ESCev\NUL\SYN0\5663\SYN\177790/U\GS\CANu,\1012312\&3\ETB[TBs\ti<\SO\14039?Y?`\45468\&3!M\EMHK\1007650\NAK}\61329\&0o;\1042207\EOT\1109945~\149894\f$\a4j\1102230\1023392v*\1078608\51776$&_bP\99509\31656DB\131583E\44212}\58030\&7\985331\1031459\45622#\USA\11919\&8&j\135104o%ms(\ETX\1093281T\1042896j\STXF\a(\33759K\99873.\ETB(\STX\21943N\1046694\178021\ENQnk\SOH\DC4\DC3s\SOH0\1032873\t\1008544T\1006265F8,\33349\ETX\DELO\US{\v<\7337\1070826\EOT>\US.\"\1005396[\DC3SE\1044074\&9D8bf\32635\&5bR\ENQ\1112871\1097678\&2k`\182237\44708\bM\37922&*\DC1F\STX\b<\1095550\985555\NUL|g?_\63870<{$L#\US\188935 \\.3\t\1061467Q\6201\1020636P\SOH\NULb~B\DC2nu\139393\EOT\1106754P\1062273\NAK\177279\1020100\ESC8\\_J*\SYN\54258n\DLE\52564\&3\119865\SOH\1095637\1108514ol\1046547\EMto>8\"\DC3`b\DC2\b\1111456a\52678\SUB9Y\ESCO$t\f;\144560.\\N(sV\STX%\1029998b\NUL9\157340\1050167{Eb\EM{\SUB\1013254\&0\1094146\CAN\1001180@\1032710\1083507M\48504\985362\1093712\&8_\r\DC3/\1019693EB\1036798\SYN\1066289YV\997690\1007559\1055442_M\STX\1074672qweO\132991w\1070167\DC4\\k\SI\1046701s9\156143\&5#\1071695\DC2>;PmFl0b\STX\175038P\SI~P!|<$\29047Ta\"j\1069035p\36712t\171493\993283\&5&RJ:h/\1038980\DLEG:=|\148164\126978\NUL}BCY\1001333\162633r\DC2\43617\998882\ESC\1002253\ACK+\1031629'\t\6543\154711\NUL\DLE&+)TvZ\1089940\1036958]GW\CANP5yp\1098337b|m\153195"
        ),
      rmCookiesLabels =
        [CookieLabel {cookieLabelText = "-\996503\SUBI\46825"}, CookieLabel {cookieLabelText = "\65109"}],
      rmCookiesIdents = [CookieId {cookieIdNum = 31}]
    }

testObject_RemoveCookies_user_19 :: RemoveCookies
testObject_RemoveCookies_user_19 =
  RemoveCookies
    { rmCookiesPassword =
        ( PlainTextPassword
            "\GS(xl[\996326\US\1041865[\1044893&;R\991026*{\1015003\1107871RXc@r\SI\1031786\&4vb\DC3I8\153057\SYN\NAK\1094392\USR\1101578P}\1104153\vn)H\b\70201'\194565\t\DC2\1059702cO5q\10271]r\1005712C\nLJ\19671ax{Ys\15238\59228|\1033537\EOTn|\1016215;D7Pq \GS'_\142437\a\EOT\1105443Zo\ENQ\DLE;3\1008922#Y?E\1065738'\138580b\1062145\100292b-?\STX~\985185V<>\DLE\n\97656O\DC2o\31775\164593y\1188\28942\33019\152481\FS\41692}w\DLE\39133\"\n\1025851i'\1061701\142441\157720\1090746\"&H\30641ypueH\9764A}no\120190\a\62252\83434-\b-\f\181491\RS\ETXGC=\1004462o9!\50322y\n\CANYf\SOH)\1014682\STX\184078uIv\1088388\&1!xUr@O\DC1,w4yg\ENQE6r\30975\72240\US\1075499-\ESC#\"!\44449\983242\1028521\990982\r\STXi|\ESC@J<[T[\1072060Y,a!\15651E\141897\134217Ej/2$AF\1105526Y\3359$JY\27926\986109\SUBI.\1081785\1040668MhzLfbA2\44394U\1015373GC$\161715\ENQ\160894.\ETX'\DEL\ETB\1084878\1062536\SUB\1063528\1088062s\DC2\US\nc\163970zTm\v\63804\boc<\RSxO3>\129566AC^+Gp\US|qdT\54796\RS\DC1QRf\DC3\n\1105984,qfsb{E\v\"{\61000c\118887\37251mV\73057P\986945A{q\991924\b\22919L\ETB{PGle\180524I5\SOHu\NAKha47e\tB\169958m\1045444\STX:\190292dp\12771\DC3\ETXE\1066415\f\DLE\RSjZ\STX/~\DC1\184505p9\1056236\437\37920\f9\991390!HG\ENQ\7060\1066531hn$\SUB\14465}\1004123\38601\\cvma\1001639IuY=PS\"`RF\GSZ\1025568\175487J\v\984836\DC3\ESC$\150920\158449~.\168686\94591\r\SUB\1082885?M\"Aff\SYN}}+\142354\1019123\&2"
        ),
      rmCookiesLabels = [CookieLabel {cookieLabelText = "ZIa"}, CookieLabel {cookieLabelText = "A+k&7"}],
      rmCookiesIdents =
        [ CookieId {cookieIdNum = 1},
          CookieId {cookieIdNum = 1},
          CookieId {cookieIdNum = 1},
          CookieId {cookieIdNum = 1},
          CookieId {cookieIdNum = 0},
          CookieId {cookieIdNum = 1}
        ]
    }

testObject_RemoveCookies_user_20 :: RemoveCookies
testObject_RemoveCookies_user_20 =
  RemoveCookies
    { rmCookiesPassword =
        ( PlainTextPassword
            "TP$.\RS\1103560X\EMWWs(AG\187449\1037444U\"\FSC\ENQ\98292>\DC4h\1032976)\SI\172025VS)\US\\\"_5\100867e\a\SUB\NUL&z\DC2}\1070160\167750j\10866UD\ETXB\GS3/\97535\1065926~\"?\164238\FSc\1072711j\EM\1017060]-\134842\1085689\178500oPU\t\171087\f;\20343\RSR<\62244\67721{T\ESC\\RpG\SO\990145.\"s\v.\DC2\DC4\DLEK2\36309\1068697\4766\15657\&7~[\121227X\US\74339nk+0!\f\\Z9P_\98842$\1008036\SOH.]H\DC3D#N{\DC1\66422Gj/T\DC1\1045049E\999678,\182670\\\GSO\a$@UP\137031yf\1075416mX\aEO@B\182439#3\EOTHy\15097\ENQ\SI0B\ETB4\ACK4c~\ENQ\137312\&3Zu\179378\SUBR3=8\3144\1113106;b~\ENQ\ENQx\1099796\&7\rLP7l\ENQ\n7\146981(\SOH\70834<\1096963U\NUL|\148870>\a\a{\b&KYr_9Ms#k\DLE#\1091276z\998907\DLEl\1022340\176581VF\9961u8G\v\f;\52352!\998238_\1044096\61267\984894R\aGA\1022828km4:|?`&0|\1050827\DC1~D\"{|\986395\&9/t\DC4l\1056011F!2\US\1005570\ESCd\DC4/p\1113468r\US{(L\183948\18218,zJ\SOH\ACKa/\SOb\f%G &6h\152825|\b\EMi.\126079\&3\FSI\1017071\ENQZY\RS\1087793K/f?-f\SO\EOTN\1083227\USx\152853\&8PM\3623d\DC2#\ENQ68c%\DC3\DC3\1103767\176028\186261t\SYN\1004211G)\nl\143832\60085A>\164469\1093537\1017120I26~\164093\140945Jp\EM.V9lQbi>\US\bng\1961\1036480\138244\165277E)@WD\ENQ\RSjA\1074933\40315\1067853\&8K-\SUBt\DC1\25843.\15887\1031699t\ACK\37200\NAK18L|\SI\ETX\EMxE\DC48j\149410\ETB\32686\&22I;}\ACKNo\20841\GSg\CANquY\1035892\a\EM+\DC3\NAKe.\STX\22922u\1110172s\1015997MD\b\1104848H\SOH\NUL;i4u8Vp\1067632o\US]i\153757p\fv\38857N\8888F|s[\153706:h\1096400J6d\NAK\16632u\166131\1045843gq"
        ),
      rmCookiesLabels =
        [ CookieLabel {cookieLabelText = "\1107621"},
          CookieLabel {cookieLabelText = ""},
          CookieLabel {cookieLabelText = "\1041960(d"}
        ],
      rmCookiesIdents =
        [ CookieId {cookieIdNum = 2},
          CookieId {cookieIdNum = 1},
          CookieId {cookieIdNum = 0},
          CookieId {cookieIdNum = 1},
          CookieId {cookieIdNum = 1}
        ]
    }
