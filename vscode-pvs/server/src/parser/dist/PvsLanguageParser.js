// Generated from PvsLanguage.g4 by ANTLR 4.5
// jshint ignore: start
var antlr4 = require('antlr4/index');
var PvsLanguageListener = require('./PvsLanguageListener').PvsLanguageListener;
var grammarFileName = "PvsLanguage.g4";

var serializedATN = ["\3\u0430\ud6d1\u8206\uad2d\u4417\uaef1\u8d80\uaadd",
    "\3Y\u030f\4\2\t\2\4\3\t\3\4\4\t\4\4\5\t\5\4\6\t\6\4\7\t\7\4\b\t\b\4",
    "\t\t\t\4\n\t\n\4\13\t\13\4\f\t\f\4\r\t\r\4\16\t\16\4\17\t\17\4\20\t",
    "\20\4\21\t\21\4\22\t\22\4\23\t\23\4\24\t\24\4\25\t\25\4\26\t\26\4\27",
    "\t\27\4\30\t\30\4\31\t\31\4\32\t\32\4\33\t\33\4\34\t\34\4\35\t\35\4",
    "\36\t\36\4\37\t\37\4 \t \4!\t!\4\"\t\"\4#\t#\4$\t$\4%\t%\4&\t&\4\'\t",
    "\'\4(\t(\4)\t)\4*\t*\4+\t+\4,\t,\4-\t-\4.\t.\4/\t/\4\60\t\60\4\61\t",
    "\61\4\62\t\62\4\63\t\63\4\64\t\64\4\65\t\65\4\66\t\66\4\67\t\67\3\2",
    "\7\2p\n\2\f\2\16\2s\13\2\3\2\3\2\3\3\3\3\3\3\3\3\3\3\7\3|\n\3\f\3\16",
    "\3\177\13\3\3\3\3\3\5\3\u0083\n\3\3\3\3\3\3\3\3\3\5\3\u0089\n\3\3\3",
    "\5\3\u008c\n\3\3\3\3\3\3\3\3\4\3\4\3\4\3\4\5\4\u0095\n\4\3\4\3\4\3\5",
    "\3\5\5\5\u009b\n\5\3\6\3\6\3\6\3\6\3\7\3\7\3\7\3\7\3\b\3\b\3\b\3\b\7",
    "\b\u00a9\n\b\f\b\16\b\u00ac\13\b\3\b\3\b\3\t\3\t\3\t\5\t\u00b3\n\t\3",
    "\n\3\n\3\n\3\n\3\n\3\13\6\13\u00bb\n\13\r\13\16\13\u00bc\3\f\3\f\3\f",
    "\5\f\u00c2\n\f\5\f\u00c4\n\f\3\r\3\r\3\r\3\r\3\r\3\r\3\r\5\r\u00cd\n",
    "\r\3\16\3\16\7\16\u00d1\n\16\f\16\16\16\u00d4\13\16\3\16\3\16\3\16\5",
    "\16\u00d9\n\16\3\17\3\17\3\17\3\17\5\17\u00df\n\17\3\20\3\20\3\20\3",
    "\20\3\20\3\21\3\21\3\21\3\21\3\21\5\21\u00eb\n\21\3\22\3\22\7\22\u00ef",
    "\n\22\f\22\16\22\u00f2\13\22\3\22\3\22\5\22\u00f6\n\22\3\22\3\22\3\22",
    "\5\22\u00fb\n\22\3\23\3\23\7\23\u00ff\n\23\f\23\16\23\u0102\13\23\3",
    "\23\3\23\3\23\3\23\3\23\3\23\3\23\3\24\3\24\3\24\3\24\5\24\u010f\n\24",
    "\3\24\3\24\3\24\3\24\3\24\7\24\u0116\n\24\f\24\16\24\u0119\13\24\3\25",
    "\3\25\3\25\3\25\3\25\3\26\3\26\3\26\3\26\5\26\u0124\n\26\3\26\3\26\3",
    "\26\3\26\5\26\u012a\n\26\7\26\u012c\n\26\f\26\16\26\u012f\13\26\3\26",
    "\3\26\3\26\3\26\3\26\3\26\7\26\u0137\n\26\f\26\16\26\u013a\13\26\3\26",
    "\3\26\5\26\u013e\n\26\3\27\3\27\3\27\3\27\3\27\3\27\3\27\5\27\u0147",
    "\n\27\3\30\3\30\3\30\3\30\3\30\6\30\u014e\n\30\r\30\16\30\u014f\3\30",
    "\3\30\3\30\3\30\3\30\3\30\3\30\3\30\7\30\u015a\n\30\f\30\16\30\u015d",
    "\13\30\3\30\3\30\3\30\3\30\3\30\3\30\7\30\u0165\n\30\f\30\16\30\u0168",
    "\13\30\3\30\3\30\3\30\3\30\3\30\3\30\7\30\u0170\n\30\f\30\16\30\u0173",
    "\13\30\3\30\3\30\3\30\3\30\3\30\3\30\6\30\u017b\n\30\r\30\16\30\u017c",
    "\3\30\3\30\5\30\u0181\n\30\3\30\3\30\3\30\6\30\u0186\n\30\r\30\16\30",
    "\u0187\3\30\3\30\3\30\3\30\3\30\3\30\3\30\3\30\3\30\3\30\3\30\7\30\u0195",
    "\n\30\f\30\16\30\u0198\13\30\3\30\3\30\7\30\u019c\n\30\f\30\16\30\u019f",
    "\13\30\3\31\3\31\3\31\3\31\3\31\3\31\3\31\3\31\3\31\7\31\u01aa\n\31",
    "\f\31\16\31\u01ad\13\31\3\31\3\31\3\31\3\31\3\31\3\31\3\31\3\31\3\31",
    "\3\31\3\31\3\31\3\31\7\31\u01bc\n\31\f\31\16\31\u01bf\13\31\3\31\3\31",
    "\3\31\3\31\5\31\u01c5\n\31\3\31\3\31\5\31\u01c9\n\31\3\32\3\32\3\32",
    "\3\32\5\32\u01cf\n\32\3\33\3\33\3\33\3\33\3\33\3\34\3\34\3\34\7\34\u01d9",
    "\n\34\f\34\16\34\u01dc\13\34\3\35\3\35\5\35\u01e0\n\35\3\36\3\36\3\36",
    "\7\36\u01e5\n\36\f\36\16\36\u01e8\13\36\3\37\3\37\3\37\3\37\3\37\5\37",
    "\u01ef\n\37\3 \3 \3 \6 \u01f4\n \r \16 \u01f5\5 \u01f8\n \3 \3 \6 \u01fc",
    "\n \r \16 \u01fd\5 \u0200\n \3!\3!\3!\6!\u0205\n!\r!\16!\u0206\5!\u0209",
    "\n!\3!\3!\6!\u020d\n!\r!\16!\u020e\5!\u0211\n!\3\"\3\"\3\"\7\"\u0216",
    "\n\"\f\"\16\"\u0219\13\"\3#\3#\3#\7#\u021e\n#\f#\16#\u0221\13#\3#\3",
    "#\3#\3$\5$\u0227\n$\3$\7$\u022a\n$\f$\16$\u022d\13$\3$\3$\5$\u0231\n",
    "$\3%\3%\5%\u0235\n%\3%\3%\6%\u0239\n%\r%\16%\u023a\3%\3%\6%\u023f\n",
    "%\r%\16%\u0240\7%\u0243\n%\f%\16%\u0246\13%\3%\3%\3%\6%\u024b\n%\r%",
    "\16%\u024c\3%\3%\6%\u0251\n%\r%\16%\u0252\7%\u0255\n%\f%\16%\u0258\13",
    "%\3%\3%\7%\u025c\n%\f%\16%\u025f\13%\3&\3&\3&\3&\3\'\3\'\3\'\3\'\3(",
    "\3(\3(\3(\7(\u026d\n(\f(\16(\u0270\13(\3(\3(\3)\5)\u0275\n)\3)\3)\3",
    ")\3)\5)\u027b\n)\3)\3)\3)\3)\3)\5)\u0282\n)\3)\7)\u0285\n)\f)\16)\u0288",
    "\13)\3)\3)\3)\3)\3*\3*\3*\3*\5*\u0292\n*\3*\3*\3*\3*\3*\5*\u0299\n*",
    "\3*\7*\u029c\n*\f*\16*\u029f\13*\3*\3*\3+\3+\3+\3+\3+\3+\3+\3+\3+\3",
    "+\3+\3+\3+\3+\5+\u02b1\n+\5+\u02b3\n+\3+\3+\3+\3+\3+\3+\5+\u02bb\n+",
    "\3,\3,\3,\5,\u02c0\n,\3,\3,\5,\u02c4\n,\3,\7,\u02c7\n,\f,\16,\u02ca",
    "\13,\3-\3-\3-\3-\7-\u02d0\n-\f-\16-\u02d3\13-\3-\3-\3.\3.\3.\3.\3/\3",
    "/\3/\3/\7/\u02df\n/\f/\16/\u02e2\13/\3\60\3\60\3\60\5\60\u02e7\n\60",
    "\3\60\3\60\5\60\u02eb\n\60\3\61\3\61\3\61\3\61\3\61\3\61\3\61\3\62\3",
    "\62\3\63\3\63\3\63\7\63\u02f9\n\63\f\63\16\63\u02fc\13\63\3\64\3\64",
    "\3\64\5\64\u0301\n\64\3\65\3\65\3\65\7\65\u0306\n\65\f\65\16\65\u0309",
    "\13\65\3\66\3\66\3\67\3\67\3\67\2\3.8\2\4\6\b\n\f\16\20\22\24\26\30",
    "\32\34\36 \"$&(*,.\60\62\64\668:<>@BDFHJLNPRTVXZ\\^`bdfhjl\2\13\3\2",
    "&(\4\2\67\67PP\3\29:\3\2AC\3\2.\60\3\2\20\21\3\2\61\62\4\2\27\33JJ\6",
    "\2\27\30\34 HIKR\u034c\2q\3\2\2\2\4v\3\2\2\2\6\u0094\3\2\2\2\b\u009a",
    "\3\2\2\2\n\u009c\3\2\2\2\f\u00a0\3\2\2\2\16\u00a4\3\2\2\2\20\u00b2\3",
    "\2\2\2\22\u00b4\3\2\2\2\24\u00ba\3\2\2\2\26\u00c3\3\2\2\2\30\u00cc\3",
    "\2\2\2\32\u00ce\3\2\2\2\34\u00da\3\2\2\2\36\u00e0\3\2\2\2 \u00e5\3\2",
    "\2\2\"\u00ec\3\2\2\2$\u00fc\3\2\2\2&\u010a\3\2\2\2(\u011a\3\2\2\2*\u013d",
    "\3\2\2\2,\u0146\3\2\2\2.\u0180\3\2\2\2\60\u01c8\3\2\2\2\62\u01ca\3\2",
    "\2\2\64\u01d0\3\2\2\2\66\u01d5\3\2\2\28\u01df\3\2\2\2:\u01e1\3\2\2\2",
    "<\u01ee\3\2\2\2>\u01f0\3\2\2\2@\u0201\3\2\2\2B\u0212\3\2\2\2D\u021a",
    "\3\2\2\2F\u0226\3\2\2\2H\u0232\3\2\2\2J\u0260\3\2\2\2L\u0264\3\2\2\2",
    "N\u0268\3\2\2\2P\u0274\3\2\2\2R\u028d\3\2\2\2T\u02ba\3\2\2\2V\u02bf",
    "\3\2\2\2X\u02cb\3\2\2\2Z\u02d6\3\2\2\2\\\u02da\3\2\2\2^\u02e6\3\2\2",
    "\2`\u02ec\3\2\2\2b\u02f3\3\2\2\2d\u02f5\3\2\2\2f\u0300\3\2\2\2h\u0302",
    "\3\2\2\2j\u030a\3\2\2\2l\u030c\3\2\2\2np\5\4\3\2on\3\2\2\2ps\3\2\2\2",
    "qo\3\2\2\2qr\3\2\2\2rt\3\2\2\2sq\3\2\2\2tu\7\2\2\3u\3\3\2\2\2v\u0082",
    "\5b\62\2wx\7\3\2\2x}\5\6\4\2yz\7\4\2\2z|\5\6\4\2{y\3\2\2\2|\177\3\2",
    "\2\2}{\3\2\2\2}~\3\2\2\2~\u0080\3\2\2\2\177}\3\2\2\2\u0080\u0081\7\5",
    "\2\2\u0081\u0083\3\2\2\2\u0082w\3\2\2\2\u0082\u0083\3\2\2\2\u0083\u0084",
    "\3\2\2\2\u0084\u0085\7\6\2\2\u0085\u0086\7!\2\2\u0086\u0088\7\"\2\2",
    "\u0087\u0089\5\16\b\2\u0088\u0087\3\2\2\2\u0088\u0089\3\2\2\2\u0089",
    "\u008b\3\2\2\2\u008a\u008c\5\24\13\2\u008b\u008a\3\2\2\2\u008b\u008c",
    "\3\2\2\2\u008c\u008d\3\2\2\2\u008d\u008e\7#\2\2\u008e\u008f\5b\62\2",
    "\u008f\5\3\2\2\2\u0090\u0091\7\7\2\2\u0091\u0092\5\\/\2\u0092\u0093",
    "\7\b\2\2\u0093\u0095\3\2\2\2\u0094\u0090\3\2\2\2\u0094\u0095\3\2\2\2",
    "\u0095\u0096\3\2\2\2\u0096\u0097\5\b\5\2\u0097\7\3\2\2\2\u0098\u009b",
    "\5\n\6\2\u0099\u009b\5\f\7\2\u009a\u0098\3\2\2\2\u009a\u0099\3\2\2\2",
    "\u009b\t\3\2\2\2\u009c\u009d\5b\62\2\u009d\u009e\7\6\2\2\u009e\u009f",
    "\t\2\2\2\u009f\13\3\2\2\2\u00a0\u00a1\5h\65\2\u00a1\u00a2\7\6\2\2\u00a2",
    "\u00a3\5,\27\2\u00a3\r\3\2\2\2\u00a4\u00a5\7E\2\2\u00a5\u00aa\5\20\t",
    "\2\u00a6\u00a7\7\t\2\2\u00a7\u00a9\5\20\t\2\u00a8\u00a6\3\2\2\2\u00a9",
    "\u00ac\3\2\2\2\u00aa\u00a8\3\2\2\2\u00aa\u00ab\3\2\2\2\u00ab\u00ad\3",
    "\2\2\2\u00ac\u00aa\3\2\2\2\u00ad\u00ae\7F\2\2\u00ae\17\3\2\2\2\u00af",
    "\u00b3\5\\/\2\u00b0\u00b3\5\22\n\2\u00b1\u00b3\5\30\r\2\u00b2\u00af",
    "\3\2\2\2\u00b2\u00b0\3\2\2\2\u00b2\u00b1\3\2\2\2\u00b3\21\3\2\2\2\u00b4",
    "\u00b5\5d\63\2\u00b5\u00b6\7\6\2\2\u00b6\u00b7\7G\2\2\u00b7\u00b8\5",
    ".\30\2\u00b8\23\3\2\2\2\u00b9\u00bb\5\26\f\2\u00ba\u00b9\3\2\2\2\u00bb",
    "\u00bc\3\2\2\2\u00bc\u00ba\3\2\2\2\u00bc\u00bd\3\2\2\2\u00bd\25\3\2",
    "\2\2\u00be\u00c4\5\\/\2\u00bf\u00c1\5\30\r\2\u00c0\u00c2\7\t\2\2\u00c1",
    "\u00c0\3\2\2\2\u00c1\u00c2\3\2\2\2\u00c2\u00c4\3\2\2\2\u00c3\u00be\3",
    "\2\2\2\u00c3\u00bf\3\2\2\2\u00c4\27\3\2\2\2\u00c5\u00cd\5\32\16\2\u00c6",
    "\u00cd\5\36\20\2\u00c7\u00cd\5 \21\2\u00c8\u00cd\5\"\22\2\u00c9\u00cd",
    "\5$\23\2\u00ca\u00cd\5&\24\2\u00cb\u00cd\5(\25\2\u00cc\u00c5\3\2\2\2",
    "\u00cc\u00c6\3\2\2\2\u00cc\u00c7\3\2\2\2\u00cc\u00c8\3\2\2\2\u00cc\u00c9",
    "\3\2\2\2\u00cc\u00ca\3\2\2\2\u00cc\u00cb\3\2\2\2\u00cd\31\3\2\2\2\u00ce",
    "\u00d2\5d\63\2\u00cf\u00d1\5*\26\2\u00d0\u00cf\3\2\2\2\u00d1\u00d4\3",
    "\2\2\2\u00d2\u00d0\3\2\2\2\u00d2\u00d3\3\2\2\2\u00d3\u00d5\3\2\2\2\u00d4",
    "\u00d2\3\2\2\2\u00d5\u00d6\7\6\2\2\u00d6\u00d8\t\2\2\2\u00d7\u00d9\5",
    "\34\17\2\u00d8\u00d7\3\2\2\2\u00d8\u00d9\3\2\2\2\u00d9\33\3\2\2\2\u00da",
    "\u00db\t\3\2\2\u00db\u00de\5,\27\2\u00dc\u00dd\78\2\2\u00dd\u00df\5",
    ".\30\2\u00de\u00dc\3\2\2\2\u00de\u00df\3\2\2\2\u00df\35\3\2\2\2\u00e0",
    "\u00e1\5d\63\2\u00e1\u00e2\7\6\2\2\u00e2\u00e3\7D\2\2\u00e3\u00e4\5",
    ".\30\2\u00e4\37\3\2\2\2\u00e5\u00e6\5h\65\2\u00e6\u00e7\7\6\2\2\u00e7",
    "\u00ea\5,\27\2\u00e8\u00e9\7P\2\2\u00e9\u00eb\5.\30\2\u00ea\u00e8\3",
    "\2\2\2\u00ea\u00eb\3\2\2\2\u00eb!\3\2\2\2\u00ec\u00f0\5f\64\2\u00ed",
    "\u00ef\5*\26\2\u00ee\u00ed\3\2\2\2\u00ef\u00f2\3\2\2\2\u00f0\u00ee\3",
    "\2\2\2\u00f0\u00f1\3\2\2\2\u00f1\u00f3\3\2\2\2\u00f2\u00f0\3\2\2\2\u00f3",
    "\u00f5\7\6\2\2\u00f4\u00f6\t\4\2\2\u00f5\u00f4\3\2\2\2\u00f5\u00f6\3",
    "\2\2\2\u00f6\u00f7\3\2\2\2\u00f7\u00fa\5,\27\2\u00f8\u00f9\7P\2\2\u00f9",
    "\u00fb\5.\30\2\u00fa\u00f8\3\2\2\2\u00fa\u00fb\3\2\2\2\u00fb#\3\2\2",
    "\2\u00fc\u0100\5f\64\2\u00fd\u00ff\5*\26\2\u00fe\u00fd\3\2\2\2\u00ff",
    "\u0102\3\2\2\2\u0100\u00fe\3\2\2\2\u0100\u0101\3\2\2\2\u0101\u0103\3",
    "\2\2\2\u0102\u0100\3\2\2\2\u0103\u0104\7\6\2\2\u0104\u0105\7\64\2\2",
    "\u0105\u0106\5,\27\2\u0106\u0107\7P\2\2\u0107\u0108\5.\30\2\u0108\u0109",
    "\5\62\32\2\u0109%\3\2\2\2\u010a\u010b\t\5\2\2\u010b\u010e\5V,\2\u010c",
    "\u010d\7\6\2\2\u010d\u010f\5,\27\2\u010e\u010c\3\2\2\2\u010e\u010f\3",
    "\2\2\2\u010f\u0117\3\2\2\2\u0110\u0111\7\4\2\2\u0111\u0112\5V,\2\u0112",
    "\u0113\7\6\2\2\u0113\u0114\5,\27\2\u0114\u0116\3\2\2\2\u0115\u0110\3",
    "\2\2\2\u0116\u0119\3\2\2\2\u0117\u0115\3\2\2\2\u0117\u0118\3\2\2\2\u0118",
    "\'\3\2\2\2\u0119\u0117\3\2\2\2\u011a\u011b\5h\65\2\u011b\u011c\7\6\2",
    "\2\u011c\u011d\7\63\2\2\u011d\u011e\5,\27\2\u011e)\3\2\2\2\u011f\u0120",
    "\7\7\2\2\u0120\u0123\5d\63\2\u0121\u0122\7\6\2\2\u0122\u0124\5.\30\2",
    "\u0123\u0121\3\2\2\2\u0123\u0124\3\2\2\2\u0124\u012d\3\2\2\2\u0125\u0126",
    "\7\4\2\2\u0126\u0129\5d\63\2\u0127\u0128\7\6\2\2\u0128\u012a\5.\30\2",
    "\u0129\u0127\3\2\2\2\u0129\u012a\3\2\2\2\u012a\u012c\3\2\2\2\u012b\u0125",
    "\3\2\2\2\u012c\u012f\3\2\2\2\u012d\u012b\3\2\2\2\u012d\u012e\3\2\2\2",
    "\u012e\u0130\3\2\2\2\u012f\u012d\3\2\2\2\u0130\u0131\7\b\2\2\u0131\u013e",
    "\3\2\2\2\u0132\u0133\7\7\2\2\u0133\u0138\5T+\2\u0134\u0135\7\4\2\2\u0135",
    "\u0137\5T+\2\u0136\u0134\3\2\2\2\u0137\u013a\3\2\2\2\u0138\u0136\3\2",
    "\2\2\u0138\u0139\3\2\2\2\u0139\u013b\3\2\2\2\u013a\u0138\3\2\2\2\u013b",
    "\u013c\7\b\2\2\u013c\u013e\3\2\2\2\u013d\u011f\3\2\2\2\u013d\u0132\3",
    "\2\2\2\u013e+\3\2\2\2\u013f\u0147\5Z.\2\u0140\u0147\5N(\2\u0141\u0147",
    "\5R*\2\u0142\u0147\5P)\2\u0143\u0147\5L\'\2\u0144\u0147\5T+\2\u0145",
    "\u0147\5V,\2\u0146\u013f\3\2\2\2\u0146\u0140\3\2\2\2\u0146\u0141\3\2",
    "\2\2\u0146\u0142\3\2\2\2\u0146\u0143\3\2\2\2\u0146\u0144\3\2\2\2\u0146",
    "\u0145\3\2\2\2\u0147-\3\2\2\2\u0148\u0149\b\30\1\2\u0149\u0181\5H%\2",
    "\u014a\u0181\5,\27\2\u014b\u014d\5j\66\2\u014c\u014e\5.\30\2\u014d\u014c",
    "\3\2\2\2\u014e\u014f\3\2\2\2\u014f\u014d\3\2\2\2\u014f\u0150\3\2\2\2",
    "\u0150\u0181\3\2\2\2\u0151\u0181\7S\2\2\u0152\u0181\5V,\2\u0153\u0181",
    "\5\60\31\2\u0154\u0181\5\64\33\2\u0155\u0156\7\n\2\2\u0156\u015b\5.",
    "\30\2\u0157\u0158\7\4\2\2\u0158\u015a\5.\30\2\u0159\u0157\3\2\2\2\u015a",
    "\u015d\3\2\2\2\u015b\u0159\3\2\2\2\u015b\u015c\3\2\2\2\u015c\u015e\3",
    "\2\2\2\u015d\u015b\3\2\2\2\u015e\u015f\7\13\2\2\u015f\u0181\3\2\2\2",
    "\u0160\u0161\7\f\2\2\u0161\u0166\5J&\2\u0162\u0163\7\4\2\2\u0163\u0165",
    "\5J&\2\u0164\u0162\3\2\2\2\u0165\u0168\3\2\2\2\u0166\u0164\3\2\2\2\u0166",
    "\u0167\3\2\2\2\u0167\u0169\3\2\2\2\u0168\u0166\3\2\2\2\u0169\u016a\7",
    "\r\2\2\u016a\u0181\3\2\2\2\u016b\u016c\7\7\2\2\u016c\u0171\5.\30\2\u016d",
    "\u016e\7\4\2\2\u016e\u0170\5.\30\2\u016f\u016d\3\2\2\2\u0170\u0173\3",
    "\2\2\2\u0171\u016f\3\2\2\2\u0171\u0172\3\2\2\2\u0172\u0174\3\2\2\2\u0173",
    "\u0171\3\2\2\2\u0174\u0175\7\b\2\2\u0175\u0181\3\2\2\2\u0176\u0177\7",
    ";\2\2\u0177\u0178\5B\"\2\u0178\u017a\7<\2\2\u0179\u017b\5.\30\2\u017a",
    "\u0179\3\2\2\2\u017b\u017c\3\2\2\2\u017c\u017a\3\2\2\2\u017c\u017d\3",
    "\2\2\2\u017d\u0181\3\2\2\2\u017e\u0181\7U\2\2\u017f\u0181\7T\2\2\u0180",
    "\u0148\3\2\2\2\u0180\u014a\3\2\2\2\u0180\u014b\3\2\2\2\u0180\u0151\3",
    "\2\2\2\u0180\u0152\3\2\2\2\u0180\u0153\3\2\2\2\u0180\u0154\3\2\2\2\u0180",
    "\u0155\3\2\2\2\u0180\u0160\3\2\2\2\u0180\u016b\3\2\2\2\u0180\u0176\3",
    "\2\2\2\u0180\u017e\3\2\2\2\u0180\u017f\3\2\2\2\u0181\u019d\3\2\2\2\u0182",
    "\u0183\f\23\2\2\u0183\u0185\5l\67\2\u0184\u0186\5.\30\2\u0185\u0184",
    "\3\2\2\2\u0186\u0187\3\2\2\2\u0187\u0185\3\2\2\2\u0187\u0188\3\2\2\2",
    "\u0188\u019c\3\2\2\2\u0189\u018a\f\22\2\2\u018a\u019c\5*\26\2\u018b",
    "\u018c\f\6\2\2\u018c\u018d\7=\2\2\u018d\u019c\5B\"\2\u018e\u018f\f\5",
    "\2\2\u018f\u0190\7>\2\2\u0190\u0191\7\3\2\2\u0191\u0196\5J&\2\u0192",
    "\u0193\7\4\2\2\u0193\u0195\5J&\2\u0194\u0192\3\2\2\2\u0195\u0198\3\2",
    "\2\2\u0196\u0194\3\2\2\2\u0196\u0197\3\2\2\2\u0197\u0199\3\2\2\2\u0198",
    "\u0196\3\2\2\2\u0199\u019a\7\5\2\2\u019a\u019c\3\2\2\2\u019b\u0182\3",
    "\2\2\2\u019b\u0189\3\2\2\2\u019b\u018b\3\2\2\2\u019b\u018e\3\2\2\2\u019c",
    "\u019f\3\2\2\2\u019d\u019b\3\2\2\2\u019d\u019e\3\2\2\2\u019e/\3\2\2",
    "\2\u019f\u019d\3\2\2\2\u01a0\u01a1\7)\2\2\u01a1\u01a2\5.\30\2\u01a2",
    "\u01a3\7*\2\2\u01a3\u01ab\5.\30\2\u01a4\u01a5\7,\2\2\u01a5\u01a6\5.",
    "\30\2\u01a6\u01a7\7*\2\2\u01a7\u01a8\5.\30\2\u01a8\u01aa\3\2\2\2\u01a9",
    "\u01a4\3\2\2\2\u01aa\u01ad\3\2\2\2\u01ab\u01a9\3\2\2\2\u01ab\u01ac\3",
    "\2\2\2\u01ac\u01ae\3\2\2\2\u01ad\u01ab\3\2\2\2\u01ae\u01af\7+\2\2\u01af",
    "\u01b0\5.\30\2\u01b0\u01b1\7-\2\2\u01b1\u01c9\3\2\2\2\u01b2\u01b3\7",
    "?\2\2\u01b3\u01b4\5.\30\2\u01b4\u01b5\7\16\2\2\u01b5\u01bd\5.\30\2\u01b6",
    "\u01b7\7\4\2\2\u01b7\u01b8\5.\30\2\u01b8\u01b9\7\16\2\2\u01b9\u01ba",
    "\5.\30\2\u01ba\u01bc\3\2\2\2\u01bb\u01b6\3\2\2\2\u01bc\u01bf\3\2\2\2",
    "\u01bd\u01bb\3\2\2\2\u01bd\u01be\3\2\2\2\u01be\u01c4\3\2\2\2\u01bf\u01bd",
    "\3\2\2\2\u01c0\u01c1\7\4\2\2\u01c1\u01c2\7+\2\2\u01c2\u01c3\7\16\2\2",
    "\u01c3\u01c5\5.\30\2\u01c4\u01c0\3\2\2\2\u01c4\u01c5\3\2\2\2\u01c5\u01c6",
    "\3\2\2\2\u01c6\u01c7\7@\2\2\u01c7\u01c9\3\2\2\2\u01c8\u01a0\3\2\2\2",
    "\u01c8\u01b2\3\2\2\2\u01c9\61\3\2\2\2\u01ca\u01cb\7\65\2\2\u01cb\u01ce",
    "\5.\30\2\u01cc\u01cd\7\66\2\2\u01cd\u01cf\5.\30\2\u01ce\u01cc\3\2\2",
    "\2\u01ce\u01cf\3\2\2\2\u01cf\63\3\2\2\2\u01d0\u01d1\t\6\2\2\u01d1\u01d2",
    "\5\66\34\2\u01d2\u01d3\7\6\2\2\u01d3\u01d4\5.\30\2\u01d4\65\3\2\2\2",
    "\u01d5\u01da\58\35\2\u01d6\u01d7\7\4\2\2\u01d7\u01d9\58\35\2\u01d8\u01d6",
    "\3\2\2\2\u01d9\u01dc\3\2\2\2\u01da\u01d8\3\2\2\2\u01da\u01db\3\2\2\2",
    "\u01db\67\3\2\2\2\u01dc\u01da\3\2\2\2\u01dd\u01e0\5f\64\2\u01de\u01e0",
    "\5:\36\2\u01df\u01dd\3\2\2\2\u01df\u01de\3\2\2\2\u01e09\3\2\2\2\u01e1",
    "\u01e6\5<\37\2\u01e2\u01e3\7\4\2\2\u01e3\u01e5\5<\37\2\u01e4\u01e2\3",
    "\2\2\2\u01e5\u01e8\3\2\2\2\u01e6\u01e4\3\2\2\2\u01e6\u01e7\3\2\2\2\u01e7",
    ";\3\2\2\2\u01e8\u01e6\3\2\2\2\u01e9\u01ef\5> \2\u01ea\u01eb\7\7\2\2",
    "\u01eb\u01ec\5@!\2\u01ec\u01ed\7\b\2\2\u01ed\u01ef\3\2\2\2\u01ee\u01e9",
    "\3\2\2\2\u01ee\u01ea\3\2\2\2\u01ef=\3\2\2\2\u01f0\u01f7\5f\64\2\u01f1",
    "\u01f3\7\6\2\2\u01f2\u01f4\5.\30\2\u01f3\u01f2\3\2\2\2\u01f4\u01f5\3",
    "\2\2\2\u01f5\u01f3\3\2\2\2\u01f5\u01f6\3\2\2\2\u01f6\u01f8\3\2\2\2\u01f7",
    "\u01f1\3\2\2\2\u01f7\u01f8\3\2\2\2\u01f8\u01ff\3\2\2\2\u01f9\u01fb\7",
    "\17\2\2\u01fa\u01fc\5.\30\2\u01fb\u01fa\3\2\2\2\u01fc\u01fd\3\2\2\2",
    "\u01fd\u01fb\3\2\2\2\u01fd\u01fe\3\2\2\2\u01fe\u0200\3\2\2\2\u01ff\u01f9",
    "\3\2\2\2\u01ff\u0200\3\2\2\2\u0200?\3\2\2\2\u0201\u0208\5h\65\2\u0202",
    "\u0204\7\6\2\2\u0203\u0205\5.\30\2\u0204\u0203\3\2\2\2\u0205\u0206\3",
    "\2\2\2\u0206\u0204\3\2\2\2\u0206\u0207\3\2\2\2\u0207\u0209\3\2\2\2\u0208",
    "\u0202\3\2\2\2\u0208\u0209\3\2\2\2\u0209\u0210\3\2\2\2\u020a\u020c\7",
    "\17\2\2\u020b\u020d\5.\30\2\u020c\u020b\3\2\2\2\u020d\u020e\3\2\2\2",
    "\u020e\u020c\3\2\2\2\u020e\u020f\3\2\2\2\u020f\u0211\3\2\2\2\u0210\u020a",
    "\3\2\2\2\u0210\u0211\3\2\2\2\u0211A\3\2\2\2\u0212\u0217\5D#\2\u0213",
    "\u0214\7\4\2\2\u0214\u0216\5D#\2\u0215\u0213\3\2\2\2\u0216\u0219\3\2",
    "\2\2\u0217\u0215\3\2\2\2\u0217\u0218\3\2\2\2\u0218C\3\2\2\2\u0219\u0217",
    "\3\2\2\2\u021a\u021f\5F$\2\u021b\u021c\7\4\2\2\u021c\u021e\5F$\2\u021d",
    "\u021b\3\2\2\2\u021e\u0221\3\2\2\2\u021f\u021d\3\2\2\2\u021f\u0220\3",
    "\2\2\2\u0220\u0222\3\2\2\2\u0221\u021f\3\2\2\2\u0222\u0223\7P\2\2\u0223",
    "\u0224\5.\30\2\u0224E\3\2\2\2\u0225\u0227\5f\64\2\u0226\u0225\3\2\2",
    "\2\u0226\u0227\3\2\2\2\u0227\u022b\3\2\2\2\u0228\u022a\5:\36\2\u0229",
    "\u0228\3\2\2\2\u022a\u022d\3\2\2\2\u022b\u0229\3\2\2\2\u022b\u022c\3",
    "\2\2\2\u022c\u0230\3\2\2\2\u022d\u022b\3\2\2\2\u022e\u022f\7\6\2\2\u022f",
    "\u0231\5.\30\2\u0230\u022e\3\2\2\2\u0230\u0231\3\2\2\2\u0231G\3\2\2",
    "\2\u0232\u0234\5b\62\2\u0233\u0235\5X-\2\u0234\u0233\3\2\2\2\u0234\u0235",
    "\3\2\2\2\u0235\u0236\3\2\2\2\u0236\u0238\7\7\2\2\u0237\u0239\5.\30\2",
    "\u0238\u0237\3\2\2\2\u0239\u023a\3\2\2\2\u023a\u0238\3\2\2\2\u023a\u023b",
    "\3\2\2\2\u023b\u0244\3\2\2\2\u023c\u023e\7\4\2\2\u023d\u023f\5.\30\2",
    "\u023e\u023d\3\2\2\2\u023f\u0240\3\2\2\2\u0240\u023e\3\2\2\2\u0240\u0241",
    "\3\2\2\2\u0241\u0243\3\2\2\2\u0242\u023c\3\2\2\2\u0243\u0246\3\2\2\2",
    "\u0244\u0242\3\2\2\2\u0244\u0245\3\2\2\2\u0245\u0247\3\2\2\2\u0246\u0244",
    "\3\2\2\2\u0247\u025d\7\b\2\2\u0248\u024a\7\7\2\2\u0249\u024b\5.\30\2",
    "\u024a\u0249\3\2\2\2\u024b\u024c\3\2\2\2\u024c\u024a\3\2\2\2\u024c\u024d",
    "\3\2\2\2\u024d\u0256\3\2\2\2\u024e\u0250\7\4\2\2\u024f\u0251\5.\30\2",
    "\u0250\u024f\3\2\2\2\u0251\u0252\3\2\2\2\u0252\u0250\3\2\2\2\u0252\u0253",
    "\3\2\2\2\u0253\u0255\3\2\2\2\u0254\u024e\3\2\2\2\u0255\u0258\3\2\2\2",
    "\u0256\u0254\3\2\2\2\u0256\u0257\3\2\2\2\u0257\u0259\3\2\2\2\u0258\u0256",
    "\3\2\2\2\u0259\u025a\7\b\2\2\u025a\u025c\3\2\2\2\u025b\u0248\3\2\2\2",
    "\u025c\u025f\3\2\2\2\u025d\u025b\3\2\2\2\u025d\u025e\3\2\2\2\u025eI",
    "\3\2\2\2\u025f\u025d\3\2\2\2\u0260\u0261\5b\62\2\u0261\u0262\t\7\2\2",
    "\u0262\u0263\5.\30\2\u0263K\3\2\2\2\u0264\u0265\5b\62\2\u0265\u0266",
    "\7\6\2\2\u0266\u0267\5.\30\2\u0267M\3\2\2\2\u0268\u0269\7\22\2\2\u0269",
    "\u026e\5L\'\2\u026a\u026b\7\4\2\2\u026b\u026d\5L\'\2\u026c\u026a\3\2",
    "\2\2\u026d\u0270\3\2\2\2\u026e\u026c\3\2\2\2\u026e\u026f\3\2\2\2\u026f",
    "\u0271\3\2\2\2\u0270\u026e\3\2\2\2\u0271\u0272\7\23\2\2\u0272O\3\2\2",
    "\2\u0273\u0275\t\b\2\2\u0274\u0273\3\2\2\2\u0274\u0275\3\2\2\2\u0275",
    "\u0276\3\2\2\2\u0276\u027a\7\3\2\2\u0277\u0278\5h\65\2\u0278\u0279\7",
    "\6\2\2\u0279\u027b\3\2\2\2\u027a\u0277\3\2\2\2\u027a\u027b\3\2\2\2\u027b",
    "\u027c\3\2\2\2\u027c\u0286\5,\27\2\u027d\u0281\7\4\2\2\u027e\u027f\5",
    "h\65\2\u027f\u0280\7\6\2\2\u0280\u0282\3\2\2\2\u0281\u027e\3\2\2\2\u0281",
    "\u0282\3\2\2\2\u0282\u0283\3\2\2\2\u0283\u0285\5,\27\2\u0284\u027d\3",
    "\2\2\2\u0285\u0288\3\2\2\2\u0286\u0284\3\2\2\2\u0286\u0287\3\2\2\2\u0287",
    "\u0289\3\2\2\2\u0288\u0286\3\2\2\2\u0289\u028a\7\16\2\2\u028a\u028b",
    "\5,\27\2\u028b\u028c\7\5\2\2\u028cQ\3\2\2\2\u028d\u0291\7\3\2\2\u028e",
    "\u028f\5h\65\2\u028f\u0290\7\6\2\2\u0290\u0292\3\2\2\2\u0291\u028e\3",
    "\2\2\2\u0291\u0292\3\2\2\2\u0292\u0293\3\2\2\2\u0293\u029d\5,\27\2\u0294",
    "\u0298\7\4\2\2\u0295\u0296\5h\65\2\u0296\u0297\7\6\2\2\u0297\u0299\3",
    "\2\2\2\u0298\u0295\3\2\2\2\u0298\u0299\3\2\2\2\u0299\u029a\3\2\2\2\u029a",
    "\u029c\5,\27\2\u029b\u0294\3\2\2\2\u029c\u029f\3\2\2\2\u029d\u029b\3",
    "\2\2\2\u029d\u029e\3\2\2\2\u029e\u02a0\3\2\2\2\u029f\u029d\3\2\2\2\u02a0",
    "\u02a1\7\5\2\2\u02a1S\3\2\2\2\u02a2\u02a3\7\24\2\2\u02a3\u02a4\5d\63",
    "\2\u02a4\u02a5\7\6\2\2\u02a5\u02a6\5.\30\2\u02a6\u02a7\7\17\2\2\u02a7",
    "\u02a8\5.\30\2\u02a8\u02a9\7\25\2\2\u02a9\u02bb\3\2\2\2\u02aa\u02ab",
    "\7\7\2\2\u02ab\u02b2\5d\63\2\u02ac\u02ad\7\6\2\2\u02ad\u02b0\5.\30\2",
    "\u02ae\u02af\7\17\2\2\u02af\u02b1\5.\30\2\u02b0\u02ae\3\2\2\2\u02b0",
    "\u02b1\3\2\2\2\u02b1\u02b3\3\2\2\2\u02b2\u02ac\3\2\2\2\u02b2\u02b3\3",
    "\2\2\2\u02b3\u02b4\3\2\2\2\u02b4\u02b5\7\b\2\2\u02b5\u02bb\3\2\2\2\u02b6",
    "\u02b7\7\7\2\2\u02b7\u02b8\5.\30\2\u02b8\u02b9\7\b\2\2\u02b9\u02bb\3",
    "\2\2\2\u02ba\u02a2\3\2\2\2\u02ba\u02aa\3\2\2\2\u02ba\u02b6\3\2\2\2\u02bb",
    "U\3\2\2\2\u02bc\u02bd\5b\62\2\u02bd\u02be\7\26\2\2\u02be\u02c0\3\2\2",
    "\2\u02bf\u02bc\3\2\2\2\u02bf\u02c0\3\2\2\2\u02c0\u02c1\3\2\2\2\u02c1",
    "\u02c3\5f\64\2\u02c2\u02c4\5X-\2\u02c3\u02c2\3\2\2\2\u02c3\u02c4\3\2",
    "\2\2\u02c4\u02c8\3\2\2\2\u02c5\u02c7\5*\26\2\u02c6\u02c5\3\2\2\2\u02c7",
    "\u02ca\3\2\2\2\u02c8\u02c6\3\2\2\2\u02c8\u02c9\3\2\2\2\u02c9W\3\2\2",
    "\2\u02ca\u02c8\3\2\2\2\u02cb\u02cc\7\3\2\2\u02cc\u02d1\5.\30\2\u02cd",
    "\u02ce\7\4\2\2\u02ce\u02d0\5.\30\2\u02cf\u02cd\3\2\2\2\u02d0\u02d3\3",
    "\2\2\2\u02d1\u02cf\3\2\2\2\u02d1\u02d2\3\2\2\2\u02d2\u02d4\3\2\2\2\u02d3",
    "\u02d1\3\2\2\2\u02d4\u02d5\7\5\2\2\u02d5Y\3\2\2\2\u02d6\u02d7\7\24\2",
    "\2\u02d7\u02d8\5h\65\2\u02d8\u02d9\7\25\2\2\u02d9[\3\2\2\2\u02da\u02db",
    "\7%\2\2\u02db\u02e0\5^\60\2\u02dc\u02dd\7\4\2\2\u02dd\u02df\5^\60\2",
    "\u02de\u02dc\3\2\2\2\u02df\u02e2\3\2\2\2\u02e0\u02de\3\2\2\2\u02e0\u02e1",
    "\3\2\2\2\u02e1]\3\2\2\2\u02e2\u02e0\3\2\2\2\u02e3\u02e4\5b\62\2\u02e4",
    "\u02e5\7\26\2\2\u02e5\u02e7\3\2\2\2\u02e6\u02e3\3\2\2\2\u02e6\u02e7",
    "\3\2\2\2\u02e7\u02e8\3\2\2\2\u02e8\u02ea\5b\62\2\u02e9\u02eb\5X-\2\u02ea",
    "\u02e9\3\2\2\2\u02ea\u02eb\3\2\2\2\u02eb_\3\2\2\2\u02ec\u02ed\5b\62",
    "\2\u02ed\u02ee\7\6\2\2\u02ee\u02ef\7$\2\2\u02ef\u02f0\7\"\2\2\u02f0",
    "\u02f1\7#\2\2\u02f1\u02f2\5b\62\2\u02f2a\3\2\2\2\u02f3\u02f4\7V\2\2",
    "\u02f4c\3\2\2\2\u02f5\u02fa\5b\62\2\u02f6\u02f7\7\4\2\2\u02f7\u02f9",
    "\5b\62\2\u02f8\u02f6\3\2\2\2\u02f9\u02fc\3\2\2\2\u02fa\u02f8\3\2\2\2",
    "\u02fa\u02fb\3\2\2\2\u02fbe\3\2\2\2\u02fc\u02fa\3\2\2\2\u02fd\u0301",
    "\5b\62\2\u02fe\u0301\5j\66\2\u02ff\u0301\5l\67\2\u0300\u02fd\3\2\2\2",
    "\u0300\u02fe\3\2\2\2\u0300\u02ff\3\2\2\2\u0301g\3\2\2\2\u0302\u0307",
    "\5f\64\2\u0303\u0304\7\4\2\2\u0304\u0306\5f\64\2\u0305\u0303\3\2\2\2",
    "\u0306\u0309\3\2\2\2\u0307\u0305\3\2\2\2\u0307\u0308\3\2\2\2\u0308i",
    "\3\2\2\2\u0309\u0307\3\2\2\2\u030a\u030b\t\t\2\2\u030bk\3\2\2\2\u030c",
    "\u030d\t\n\2\2\u030dm\3\2\2\2\\q}\u0082\u0088\u008b\u0094\u009a\u00aa",
    "\u00b2\u00bc\u00c1\u00c3\u00cc\u00d2\u00d8\u00de\u00ea\u00f0\u00f5\u00fa",
    "\u0100\u010e\u0117\u0123\u0129\u012d\u0138\u013d\u0146\u014f\u015b\u0166",
    "\u0171\u017c\u0180\u0187\u0196\u019b\u019d\u01ab\u01bd\u01c4\u01c8\u01ce",
    "\u01da\u01df\u01e6\u01ee\u01f5\u01f7\u01fd\u01ff\u0206\u0208\u020e\u0210",
    "\u0217\u021f\u0226\u022b\u0230\u0234\u023a\u0240\u0244\u024c\u0252\u0256",
    "\u025d\u026e\u0274\u027a\u0281\u0286\u0291\u0298\u029d\u02b0\u02b2\u02ba",
    "\u02bf\u02c3\u02c8\u02d1\u02e0\u02e6\u02ea\u02fa\u0300\u0307"].join("");


var atn = new antlr4.atn.ATNDeserializer().deserialize(serializedATN);

var decisionsToDFA = atn.decisionToState.map( function(ds, index) { return new antlr4.dfa.DFA(ds, index); });

var sharedContextCache = new antlr4.PredictionContextCache();

var literalNames = [ 'null', "'['", "','", "']'", "':'", "'('", "')'", "';'", 
                     "'(:'", "':)'", "'(#'", "'#)'", "'->'", "'|'", "':='", 
                     "'|->'", "'[#'", "'#]'", "'{'", "'}'", "'@'", "'+'", 
                     "'-'", "'~'", "'[]'", "'<>'", "'`'", "'*'", "'/'", 
                     "'<'", "'>'", 'null', 'null', 'null', 'null', 'null', 
                     'null', 'null', 'null', 'null', 'null', 'null', 'null', 
                     'null', 'null', 'null', 'null', 'null', 'null', 'null', 
                     'null', 'null', 'null', 'null', 'null', 'null', 'null', 
                     'null', 'null', 'null', 'null', 'null', 'null', 'null', 
                     'null', 'null', 'null', 'null', 'null', 'null', 'null', 
                     'null', 'null', 'null', 'null', 'null', 'null', 'null', 
                     "'='", "'^'", "'o'" ];

var symbolicNames = [ 'null', 'null', 'null', 'null', 'null', 'null', 'null', 
                      'null', 'null', 'null', 'null', 'null', 'null', 'null', 
                      'null', 'null', 'null', 'null', 'null', 'null', 'null', 
                      'null', 'null', 'null', 'null', 'null', 'null', 'null', 
                      'null', 'null', 'null', "K_THEORY", "K_BEGIN", "K_END", 
                      "K_DATATYPE", "K_IMPORTING", "K_TYPE", "K_NONEMPTY_TYPE", 
                      "K_TYPE_PLUS", "K_IF", "K_THEN", "K_ELSE", "K_ELSIF", 
                      "K_ENDIF", "K_LAMBDA", "K_FORALL", "K_EXISTS", "K_FUNCTION", 
                      "K_ARRAY", "K_VAR", "K_RECURSIVE", "K_MEASURE", "K_BY", 
                      "K_FROM", "K_CONTAINING", "K_MACRO", "K_INDUCTIVE", 
                      "K_LET", "K_IN", "K_WHERE", "K_WITH", "K_COND", "K_ENDCOND", 
                      "K_CONVERSION", "K_CONVERSION_PLUS", "K_CONVERSION_MINUS", 
                      "K_FORMULA", "K_ASSUMING", "K_ENDASSUMING", "K_ASSUMPTION", 
                      "O_IFF", "O_IMPLIES", "O_NOT", "O_AND", "O_OR", "O_NOT_EQUAL", 
                      "O_LE", "O_GE", "O_EQUAL", "O_EXP", "O_CONCAT", "TRUE_FALSE", 
                      "STRING", "NUMBER", "ID", "WS", "COMMENT", "UnrecognizedChar" ];

var ruleNames =  [ "parse", "theory", "theoryFormal", "theoryFormalDeclaration", 
                   "theoryFormalType", "theoryFormalConst", "assumingPart", 
                   "assumingElement", "assumption", "theoryPart", "theoryElement", 
                   "theoryDeclaration", "typeDeclaration", "typeDefinition", 
                   "formulaDeclaration", "constantDeclaration", "functionDeclaration", 
                   "recursiveDeclaration", "conversionDeclaration", "varDeclaration", 
                   "arguments", "typeExpression", "expr", "ifExpression", 
                   "measureExpression", "bindingExpression", "lambdaBindings", 
                   "lambdaBinding", "bindings", "binding", "typeId", "typeIds", 
                   "letBindings", "letBinding", "letBind", "functionExpression", 
                   "assignmentExpression", "bindingDeclaration", "recordType", 
                   "functionType", "tupleType", "subtype", "name", "actuals", 
                   "enumerationType", "importing", "theoryName", "datatype", 
                   "identifier", "identifiers", "identifierOrOperator", 
                   "identifierOrOperators", "unaryOp", "binaryOp" ];

function PvsLanguageParser (input) {
	antlr4.Parser.call(this, input);
    this._interp = new antlr4.atn.ParserATNSimulator(this, atn, decisionsToDFA, sharedContextCache);
    this.ruleNames = ruleNames;
    this.literalNames = literalNames;
    this.symbolicNames = symbolicNames;
    return this;
}

PvsLanguageParser.prototype = Object.create(antlr4.Parser.prototype);
PvsLanguageParser.prototype.constructor = PvsLanguageParser;

Object.defineProperty(PvsLanguageParser.prototype, "atn", {
	get : function() {
		return atn;
	}
});

PvsLanguageParser.EOF = antlr4.Token.EOF;
PvsLanguageParser.T__0 = 1;
PvsLanguageParser.T__1 = 2;
PvsLanguageParser.T__2 = 3;
PvsLanguageParser.T__3 = 4;
PvsLanguageParser.T__4 = 5;
PvsLanguageParser.T__5 = 6;
PvsLanguageParser.T__6 = 7;
PvsLanguageParser.T__7 = 8;
PvsLanguageParser.T__8 = 9;
PvsLanguageParser.T__9 = 10;
PvsLanguageParser.T__10 = 11;
PvsLanguageParser.T__11 = 12;
PvsLanguageParser.T__12 = 13;
PvsLanguageParser.T__13 = 14;
PvsLanguageParser.T__14 = 15;
PvsLanguageParser.T__15 = 16;
PvsLanguageParser.T__16 = 17;
PvsLanguageParser.T__17 = 18;
PvsLanguageParser.T__18 = 19;
PvsLanguageParser.T__19 = 20;
PvsLanguageParser.T__20 = 21;
PvsLanguageParser.T__21 = 22;
PvsLanguageParser.T__22 = 23;
PvsLanguageParser.T__23 = 24;
PvsLanguageParser.T__24 = 25;
PvsLanguageParser.T__25 = 26;
PvsLanguageParser.T__26 = 27;
PvsLanguageParser.T__27 = 28;
PvsLanguageParser.T__28 = 29;
PvsLanguageParser.T__29 = 30;
PvsLanguageParser.K_THEORY = 31;
PvsLanguageParser.K_BEGIN = 32;
PvsLanguageParser.K_END = 33;
PvsLanguageParser.K_DATATYPE = 34;
PvsLanguageParser.K_IMPORTING = 35;
PvsLanguageParser.K_TYPE = 36;
PvsLanguageParser.K_NONEMPTY_TYPE = 37;
PvsLanguageParser.K_TYPE_PLUS = 38;
PvsLanguageParser.K_IF = 39;
PvsLanguageParser.K_THEN = 40;
PvsLanguageParser.K_ELSE = 41;
PvsLanguageParser.K_ELSIF = 42;
PvsLanguageParser.K_ENDIF = 43;
PvsLanguageParser.K_LAMBDA = 44;
PvsLanguageParser.K_FORALL = 45;
PvsLanguageParser.K_EXISTS = 46;
PvsLanguageParser.K_FUNCTION = 47;
PvsLanguageParser.K_ARRAY = 48;
PvsLanguageParser.K_VAR = 49;
PvsLanguageParser.K_RECURSIVE = 50;
PvsLanguageParser.K_MEASURE = 51;
PvsLanguageParser.K_BY = 52;
PvsLanguageParser.K_FROM = 53;
PvsLanguageParser.K_CONTAINING = 54;
PvsLanguageParser.K_MACRO = 55;
PvsLanguageParser.K_INDUCTIVE = 56;
PvsLanguageParser.K_LET = 57;
PvsLanguageParser.K_IN = 58;
PvsLanguageParser.K_WHERE = 59;
PvsLanguageParser.K_WITH = 60;
PvsLanguageParser.K_COND = 61;
PvsLanguageParser.K_ENDCOND = 62;
PvsLanguageParser.K_CONVERSION = 63;
PvsLanguageParser.K_CONVERSION_PLUS = 64;
PvsLanguageParser.K_CONVERSION_MINUS = 65;
PvsLanguageParser.K_FORMULA = 66;
PvsLanguageParser.K_ASSUMING = 67;
PvsLanguageParser.K_ENDASSUMING = 68;
PvsLanguageParser.K_ASSUMPTION = 69;
PvsLanguageParser.O_IFF = 70;
PvsLanguageParser.O_IMPLIES = 71;
PvsLanguageParser.O_NOT = 72;
PvsLanguageParser.O_AND = 73;
PvsLanguageParser.O_OR = 74;
PvsLanguageParser.O_NOT_EQUAL = 75;
PvsLanguageParser.O_LE = 76;
PvsLanguageParser.O_GE = 77;
PvsLanguageParser.O_EQUAL = 78;
PvsLanguageParser.O_EXP = 79;
PvsLanguageParser.O_CONCAT = 80;
PvsLanguageParser.TRUE_FALSE = 81;
PvsLanguageParser.STRING = 82;
PvsLanguageParser.NUMBER = 83;
PvsLanguageParser.ID = 84;
PvsLanguageParser.WS = 85;
PvsLanguageParser.COMMENT = 86;
PvsLanguageParser.UnrecognizedChar = 87;

PvsLanguageParser.RULE_parse = 0;
PvsLanguageParser.RULE_theory = 1;
PvsLanguageParser.RULE_theoryFormal = 2;
PvsLanguageParser.RULE_theoryFormalDeclaration = 3;
PvsLanguageParser.RULE_theoryFormalType = 4;
PvsLanguageParser.RULE_theoryFormalConst = 5;
PvsLanguageParser.RULE_assumingPart = 6;
PvsLanguageParser.RULE_assumingElement = 7;
PvsLanguageParser.RULE_assumption = 8;
PvsLanguageParser.RULE_theoryPart = 9;
PvsLanguageParser.RULE_theoryElement = 10;
PvsLanguageParser.RULE_theoryDeclaration = 11;
PvsLanguageParser.RULE_typeDeclaration = 12;
PvsLanguageParser.RULE_typeDefinition = 13;
PvsLanguageParser.RULE_formulaDeclaration = 14;
PvsLanguageParser.RULE_constantDeclaration = 15;
PvsLanguageParser.RULE_functionDeclaration = 16;
PvsLanguageParser.RULE_recursiveDeclaration = 17;
PvsLanguageParser.RULE_conversionDeclaration = 18;
PvsLanguageParser.RULE_varDeclaration = 19;
PvsLanguageParser.RULE_arguments = 20;
PvsLanguageParser.RULE_typeExpression = 21;
PvsLanguageParser.RULE_expr = 22;
PvsLanguageParser.RULE_ifExpression = 23;
PvsLanguageParser.RULE_measureExpression = 24;
PvsLanguageParser.RULE_bindingExpression = 25;
PvsLanguageParser.RULE_lambdaBindings = 26;
PvsLanguageParser.RULE_lambdaBinding = 27;
PvsLanguageParser.RULE_bindings = 28;
PvsLanguageParser.RULE_binding = 29;
PvsLanguageParser.RULE_typeId = 30;
PvsLanguageParser.RULE_typeIds = 31;
PvsLanguageParser.RULE_letBindings = 32;
PvsLanguageParser.RULE_letBinding = 33;
PvsLanguageParser.RULE_letBind = 34;
PvsLanguageParser.RULE_functionExpression = 35;
PvsLanguageParser.RULE_assignmentExpression = 36;
PvsLanguageParser.RULE_bindingDeclaration = 37;
PvsLanguageParser.RULE_recordType = 38;
PvsLanguageParser.RULE_functionType = 39;
PvsLanguageParser.RULE_tupleType = 40;
PvsLanguageParser.RULE_subtype = 41;
PvsLanguageParser.RULE_name = 42;
PvsLanguageParser.RULE_actuals = 43;
PvsLanguageParser.RULE_enumerationType = 44;
PvsLanguageParser.RULE_importing = 45;
PvsLanguageParser.RULE_theoryName = 46;
PvsLanguageParser.RULE_datatype = 47;
PvsLanguageParser.RULE_identifier = 48;
PvsLanguageParser.RULE_identifiers = 49;
PvsLanguageParser.RULE_identifierOrOperator = 50;
PvsLanguageParser.RULE_identifierOrOperators = 51;
PvsLanguageParser.RULE_unaryOp = 52;
PvsLanguageParser.RULE_binaryOp = 53;

function ParseContext(parser, parent, invokingState) {
	if(parent===undefined) {
	    parent = null;
	}
	if(invokingState===undefined || invokingState===null) {
		invokingState = -1;
	}
	antlr4.ParserRuleContext.call(this, parent, invokingState);
    this.parser = parser;
    this.ruleIndex = PvsLanguageParser.RULE_parse;
    return this;
}

ParseContext.prototype = Object.create(antlr4.ParserRuleContext.prototype);
ParseContext.prototype.constructor = ParseContext;

ParseContext.prototype.EOF = function() {
    return this.getToken(PvsLanguageParser.EOF, 0);
};

ParseContext.prototype.theory = function(i) {
    if(i===undefined) {
        i = null;
    }
    if(i===null) {
        return this.getTypedRuleContexts(TheoryContext);
    } else {
        return this.getTypedRuleContext(TheoryContext,i);
    }
};

ParseContext.prototype.enterRule = function(listener) {
    if(listener instanceof PvsLanguageListener ) {
        listener.enterParse(this);
	}
};

ParseContext.prototype.exitRule = function(listener) {
    if(listener instanceof PvsLanguageListener ) {
        listener.exitParse(this);
	}
};




PvsLanguageParser.ParseContext = ParseContext;

PvsLanguageParser.prototype.parse = function() {

    var localctx = new ParseContext(this, this._ctx, this.state);
    this.enterRule(localctx, 0, PvsLanguageParser.RULE_parse);
    var _la = 0; // Token type
    try {
        this.enterOuterAlt(localctx, 1);
        this.state = 111;
        this._errHandler.sync(this);
        _la = this._input.LA(1);
        while(_la===PvsLanguageParser.ID) {
            this.state = 108;
            this.theory();
            this.state = 113;
            this._errHandler.sync(this);
            _la = this._input.LA(1);
        }
        this.state = 114;
        this.match(PvsLanguageParser.EOF);
    } catch (re) {
    	if(re instanceof antlr4.error.RecognitionException) {
	        localctx.exception = re;
	        this._errHandler.reportError(this, re);
	        this._errHandler.recover(this, re);
	    } else {
	    	throw re;
	    }
    } finally {
        this.exitRule();
    }
    return localctx;
};

function TheoryContext(parser, parent, invokingState) {
	if(parent===undefined) {
	    parent = null;
	}
	if(invokingState===undefined || invokingState===null) {
		invokingState = -1;
	}
	antlr4.ParserRuleContext.call(this, parent, invokingState);
    this.parser = parser;
    this.ruleIndex = PvsLanguageParser.RULE_theory;
    return this;
}

TheoryContext.prototype = Object.create(antlr4.ParserRuleContext.prototype);
TheoryContext.prototype.constructor = TheoryContext;

TheoryContext.prototype.identifier = function(i) {
    if(i===undefined) {
        i = null;
    }
    if(i===null) {
        return this.getTypedRuleContexts(IdentifierContext);
    } else {
        return this.getTypedRuleContext(IdentifierContext,i);
    }
};

TheoryContext.prototype.K_THEORY = function() {
    return this.getToken(PvsLanguageParser.K_THEORY, 0);
};

TheoryContext.prototype.K_BEGIN = function() {
    return this.getToken(PvsLanguageParser.K_BEGIN, 0);
};

TheoryContext.prototype.K_END = function() {
    return this.getToken(PvsLanguageParser.K_END, 0);
};

TheoryContext.prototype.theoryFormal = function(i) {
    if(i===undefined) {
        i = null;
    }
    if(i===null) {
        return this.getTypedRuleContexts(TheoryFormalContext);
    } else {
        return this.getTypedRuleContext(TheoryFormalContext,i);
    }
};

TheoryContext.prototype.assumingPart = function() {
    return this.getTypedRuleContext(AssumingPartContext,0);
};

TheoryContext.prototype.theoryPart = function() {
    return this.getTypedRuleContext(TheoryPartContext,0);
};

TheoryContext.prototype.enterRule = function(listener) {
    if(listener instanceof PvsLanguageListener ) {
        listener.enterTheory(this);
	}
};

TheoryContext.prototype.exitRule = function(listener) {
    if(listener instanceof PvsLanguageListener ) {
        listener.exitTheory(this);
	}
};




PvsLanguageParser.TheoryContext = TheoryContext;

PvsLanguageParser.prototype.theory = function() {

    var localctx = new TheoryContext(this, this._ctx, this.state);
    this.enterRule(localctx, 2, PvsLanguageParser.RULE_theory);
    var _la = 0; // Token type
    try {
        this.enterOuterAlt(localctx, 1);
        this.state = 116;
        this.identifier();
        this.state = 128;
        _la = this._input.LA(1);
        if(_la===PvsLanguageParser.T__0) {
            this.state = 117;
            this.match(PvsLanguageParser.T__0);
            this.state = 118;
            this.theoryFormal();
            this.state = 123;
            this._errHandler.sync(this);
            _la = this._input.LA(1);
            while(_la===PvsLanguageParser.T__1) {
                this.state = 119;
                this.match(PvsLanguageParser.T__1);
                this.state = 120;
                this.theoryFormal();
                this.state = 125;
                this._errHandler.sync(this);
                _la = this._input.LA(1);
            }
            this.state = 126;
            this.match(PvsLanguageParser.T__2);
        }

        this.state = 130;
        this.match(PvsLanguageParser.T__3);
        this.state = 131;
        this.match(PvsLanguageParser.K_THEORY);
        this.state = 132;
        this.match(PvsLanguageParser.K_BEGIN);
        this.state = 134;
        _la = this._input.LA(1);
        if(_la===PvsLanguageParser.K_ASSUMING) {
            this.state = 133;
            this.assumingPart();
        }

        this.state = 137;
        _la = this._input.LA(1);
        if(((((_la - 21)) & ~0x1f) == 0 && ((1 << (_la - 21)) & ((1 << (PvsLanguageParser.T__20 - 21)) | (1 << (PvsLanguageParser.T__21 - 21)) | (1 << (PvsLanguageParser.T__22 - 21)) | (1 << (PvsLanguageParser.T__23 - 21)) | (1 << (PvsLanguageParser.T__24 - 21)) | (1 << (PvsLanguageParser.T__25 - 21)) | (1 << (PvsLanguageParser.T__26 - 21)) | (1 << (PvsLanguageParser.T__27 - 21)) | (1 << (PvsLanguageParser.T__28 - 21)) | (1 << (PvsLanguageParser.T__29 - 21)) | (1 << (PvsLanguageParser.K_IMPORTING - 21)))) !== 0) || ((((_la - 63)) & ~0x1f) == 0 && ((1 << (_la - 63)) & ((1 << (PvsLanguageParser.K_CONVERSION - 63)) | (1 << (PvsLanguageParser.K_CONVERSION_PLUS - 63)) | (1 << (PvsLanguageParser.K_CONVERSION_MINUS - 63)) | (1 << (PvsLanguageParser.O_IFF - 63)) | (1 << (PvsLanguageParser.O_IMPLIES - 63)) | (1 << (PvsLanguageParser.O_NOT - 63)) | (1 << (PvsLanguageParser.O_AND - 63)) | (1 << (PvsLanguageParser.O_OR - 63)) | (1 << (PvsLanguageParser.O_NOT_EQUAL - 63)) | (1 << (PvsLanguageParser.O_LE - 63)) | (1 << (PvsLanguageParser.O_GE - 63)) | (1 << (PvsLanguageParser.O_EQUAL - 63)) | (1 << (PvsLanguageParser.O_EXP - 63)) | (1 << (PvsLanguageParser.O_CONCAT - 63)) | (1 << (PvsLanguageParser.ID - 63)))) !== 0)) {
            this.state = 136;
            this.theoryPart();
        }

        this.state = 139;
        this.match(PvsLanguageParser.K_END);
        this.state = 140;
        this.identifier();
    } catch (re) {
    	if(re instanceof antlr4.error.RecognitionException) {
	        localctx.exception = re;
	        this._errHandler.reportError(this, re);
	        this._errHandler.recover(this, re);
	    } else {
	    	throw re;
	    }
    } finally {
        this.exitRule();
    }
    return localctx;
};

function TheoryFormalContext(parser, parent, invokingState) {
	if(parent===undefined) {
	    parent = null;
	}
	if(invokingState===undefined || invokingState===null) {
		invokingState = -1;
	}
	antlr4.ParserRuleContext.call(this, parent, invokingState);
    this.parser = parser;
    this.ruleIndex = PvsLanguageParser.RULE_theoryFormal;
    return this;
}

TheoryFormalContext.prototype = Object.create(antlr4.ParserRuleContext.prototype);
TheoryFormalContext.prototype.constructor = TheoryFormalContext;

TheoryFormalContext.prototype.theoryFormalDeclaration = function() {
    return this.getTypedRuleContext(TheoryFormalDeclarationContext,0);
};

TheoryFormalContext.prototype.importing = function() {
    return this.getTypedRuleContext(ImportingContext,0);
};

TheoryFormalContext.prototype.enterRule = function(listener) {
    if(listener instanceof PvsLanguageListener ) {
        listener.enterTheoryFormal(this);
	}
};

TheoryFormalContext.prototype.exitRule = function(listener) {
    if(listener instanceof PvsLanguageListener ) {
        listener.exitTheoryFormal(this);
	}
};




PvsLanguageParser.TheoryFormalContext = TheoryFormalContext;

PvsLanguageParser.prototype.theoryFormal = function() {

    var localctx = new TheoryFormalContext(this, this._ctx, this.state);
    this.enterRule(localctx, 4, PvsLanguageParser.RULE_theoryFormal);
    var _la = 0; // Token type
    try {
        this.enterOuterAlt(localctx, 1);
        this.state = 146;
        _la = this._input.LA(1);
        if(_la===PvsLanguageParser.T__4) {
            this.state = 142;
            this.match(PvsLanguageParser.T__4);
            this.state = 143;
            this.importing();
            this.state = 144;
            this.match(PvsLanguageParser.T__5);
        }

        this.state = 148;
        this.theoryFormalDeclaration();
    } catch (re) {
    	if(re instanceof antlr4.error.RecognitionException) {
	        localctx.exception = re;
	        this._errHandler.reportError(this, re);
	        this._errHandler.recover(this, re);
	    } else {
	    	throw re;
	    }
    } finally {
        this.exitRule();
    }
    return localctx;
};

function TheoryFormalDeclarationContext(parser, parent, invokingState) {
	if(parent===undefined) {
	    parent = null;
	}
	if(invokingState===undefined || invokingState===null) {
		invokingState = -1;
	}
	antlr4.ParserRuleContext.call(this, parent, invokingState);
    this.parser = parser;
    this.ruleIndex = PvsLanguageParser.RULE_theoryFormalDeclaration;
    return this;
}

TheoryFormalDeclarationContext.prototype = Object.create(antlr4.ParserRuleContext.prototype);
TheoryFormalDeclarationContext.prototype.constructor = TheoryFormalDeclarationContext;

TheoryFormalDeclarationContext.prototype.theoryFormalType = function() {
    return this.getTypedRuleContext(TheoryFormalTypeContext,0);
};

TheoryFormalDeclarationContext.prototype.theoryFormalConst = function() {
    return this.getTypedRuleContext(TheoryFormalConstContext,0);
};

TheoryFormalDeclarationContext.prototype.enterRule = function(listener) {
    if(listener instanceof PvsLanguageListener ) {
        listener.enterTheoryFormalDeclaration(this);
	}
};

TheoryFormalDeclarationContext.prototype.exitRule = function(listener) {
    if(listener instanceof PvsLanguageListener ) {
        listener.exitTheoryFormalDeclaration(this);
	}
};




PvsLanguageParser.TheoryFormalDeclarationContext = TheoryFormalDeclarationContext;

PvsLanguageParser.prototype.theoryFormalDeclaration = function() {

    var localctx = new TheoryFormalDeclarationContext(this, this._ctx, this.state);
    this.enterRule(localctx, 6, PvsLanguageParser.RULE_theoryFormalDeclaration);
    try {
        this.state = 152;
        var la_ = this._interp.adaptivePredict(this._input,6,this._ctx);
        switch(la_) {
        case 1:
            this.enterOuterAlt(localctx, 1);
            this.state = 150;
            this.theoryFormalType();
            break;

        case 2:
            this.enterOuterAlt(localctx, 2);
            this.state = 151;
            this.theoryFormalConst();
            break;

        }
    } catch (re) {
    	if(re instanceof antlr4.error.RecognitionException) {
	        localctx.exception = re;
	        this._errHandler.reportError(this, re);
	        this._errHandler.recover(this, re);
	    } else {
	    	throw re;
	    }
    } finally {
        this.exitRule();
    }
    return localctx;
};

function TheoryFormalTypeContext(parser, parent, invokingState) {
	if(parent===undefined) {
	    parent = null;
	}
	if(invokingState===undefined || invokingState===null) {
		invokingState = -1;
	}
	antlr4.ParserRuleContext.call(this, parent, invokingState);
    this.parser = parser;
    this.ruleIndex = PvsLanguageParser.RULE_theoryFormalType;
    return this;
}

TheoryFormalTypeContext.prototype = Object.create(antlr4.ParserRuleContext.prototype);
TheoryFormalTypeContext.prototype.constructor = TheoryFormalTypeContext;

TheoryFormalTypeContext.prototype.identifier = function() {
    return this.getTypedRuleContext(IdentifierContext,0);
};

TheoryFormalTypeContext.prototype.K_TYPE = function() {
    return this.getToken(PvsLanguageParser.K_TYPE, 0);
};

TheoryFormalTypeContext.prototype.K_NONEMPTY_TYPE = function() {
    return this.getToken(PvsLanguageParser.K_NONEMPTY_TYPE, 0);
};

TheoryFormalTypeContext.prototype.K_TYPE_PLUS = function() {
    return this.getToken(PvsLanguageParser.K_TYPE_PLUS, 0);
};

TheoryFormalTypeContext.prototype.enterRule = function(listener) {
    if(listener instanceof PvsLanguageListener ) {
        listener.enterTheoryFormalType(this);
	}
};

TheoryFormalTypeContext.prototype.exitRule = function(listener) {
    if(listener instanceof PvsLanguageListener ) {
        listener.exitTheoryFormalType(this);
	}
};




PvsLanguageParser.TheoryFormalTypeContext = TheoryFormalTypeContext;

PvsLanguageParser.prototype.theoryFormalType = function() {

    var localctx = new TheoryFormalTypeContext(this, this._ctx, this.state);
    this.enterRule(localctx, 8, PvsLanguageParser.RULE_theoryFormalType);
    var _la = 0; // Token type
    try {
        this.enterOuterAlt(localctx, 1);
        this.state = 154;
        this.identifier();
        this.state = 155;
        this.match(PvsLanguageParser.T__3);
        this.state = 156;
        _la = this._input.LA(1);
        if(!(((((_la - 36)) & ~0x1f) == 0 && ((1 << (_la - 36)) & ((1 << (PvsLanguageParser.K_TYPE - 36)) | (1 << (PvsLanguageParser.K_NONEMPTY_TYPE - 36)) | (1 << (PvsLanguageParser.K_TYPE_PLUS - 36)))) !== 0))) {
        this._errHandler.recoverInline(this);
        }
        else {
            this.consume();
        }
    } catch (re) {
    	if(re instanceof antlr4.error.RecognitionException) {
	        localctx.exception = re;
	        this._errHandler.reportError(this, re);
	        this._errHandler.recover(this, re);
	    } else {
	    	throw re;
	    }
    } finally {
        this.exitRule();
    }
    return localctx;
};

function TheoryFormalConstContext(parser, parent, invokingState) {
	if(parent===undefined) {
	    parent = null;
	}
	if(invokingState===undefined || invokingState===null) {
		invokingState = -1;
	}
	antlr4.ParserRuleContext.call(this, parent, invokingState);
    this.parser = parser;
    this.ruleIndex = PvsLanguageParser.RULE_theoryFormalConst;
    return this;
}

TheoryFormalConstContext.prototype = Object.create(antlr4.ParserRuleContext.prototype);
TheoryFormalConstContext.prototype.constructor = TheoryFormalConstContext;

TheoryFormalConstContext.prototype.identifierOrOperators = function() {
    return this.getTypedRuleContext(IdentifierOrOperatorsContext,0);
};

TheoryFormalConstContext.prototype.typeExpression = function() {
    return this.getTypedRuleContext(TypeExpressionContext,0);
};

TheoryFormalConstContext.prototype.enterRule = function(listener) {
    if(listener instanceof PvsLanguageListener ) {
        listener.enterTheoryFormalConst(this);
	}
};

TheoryFormalConstContext.prototype.exitRule = function(listener) {
    if(listener instanceof PvsLanguageListener ) {
        listener.exitTheoryFormalConst(this);
	}
};




PvsLanguageParser.TheoryFormalConstContext = TheoryFormalConstContext;

PvsLanguageParser.prototype.theoryFormalConst = function() {

    var localctx = new TheoryFormalConstContext(this, this._ctx, this.state);
    this.enterRule(localctx, 10, PvsLanguageParser.RULE_theoryFormalConst);
    try {
        this.enterOuterAlt(localctx, 1);
        this.state = 158;
        this.identifierOrOperators();
        this.state = 159;
        this.match(PvsLanguageParser.T__3);
        this.state = 160;
        this.typeExpression();
    } catch (re) {
    	if(re instanceof antlr4.error.RecognitionException) {
	        localctx.exception = re;
	        this._errHandler.reportError(this, re);
	        this._errHandler.recover(this, re);
	    } else {
	    	throw re;
	    }
    } finally {
        this.exitRule();
    }
    return localctx;
};

function AssumingPartContext(parser, parent, invokingState) {
	if(parent===undefined) {
	    parent = null;
	}
	if(invokingState===undefined || invokingState===null) {
		invokingState = -1;
	}
	antlr4.ParserRuleContext.call(this, parent, invokingState);
    this.parser = parser;
    this.ruleIndex = PvsLanguageParser.RULE_assumingPart;
    return this;
}

AssumingPartContext.prototype = Object.create(antlr4.ParserRuleContext.prototype);
AssumingPartContext.prototype.constructor = AssumingPartContext;

AssumingPartContext.prototype.K_ASSUMING = function() {
    return this.getToken(PvsLanguageParser.K_ASSUMING, 0);
};

AssumingPartContext.prototype.assumingElement = function(i) {
    if(i===undefined) {
        i = null;
    }
    if(i===null) {
        return this.getTypedRuleContexts(AssumingElementContext);
    } else {
        return this.getTypedRuleContext(AssumingElementContext,i);
    }
};

AssumingPartContext.prototype.K_ENDASSUMING = function() {
    return this.getToken(PvsLanguageParser.K_ENDASSUMING, 0);
};

AssumingPartContext.prototype.enterRule = function(listener) {
    if(listener instanceof PvsLanguageListener ) {
        listener.enterAssumingPart(this);
	}
};

AssumingPartContext.prototype.exitRule = function(listener) {
    if(listener instanceof PvsLanguageListener ) {
        listener.exitAssumingPart(this);
	}
};




PvsLanguageParser.AssumingPartContext = AssumingPartContext;

PvsLanguageParser.prototype.assumingPart = function() {

    var localctx = new AssumingPartContext(this, this._ctx, this.state);
    this.enterRule(localctx, 12, PvsLanguageParser.RULE_assumingPart);
    var _la = 0; // Token type
    try {
        this.enterOuterAlt(localctx, 1);
        this.state = 162;
        this.match(PvsLanguageParser.K_ASSUMING);
        this.state = 163;
        this.assumingElement();
        this.state = 168;
        this._errHandler.sync(this);
        _la = this._input.LA(1);
        while(_la===PvsLanguageParser.T__6) {
            this.state = 164;
            this.match(PvsLanguageParser.T__6);
            this.state = 165;
            this.assumingElement();
            this.state = 170;
            this._errHandler.sync(this);
            _la = this._input.LA(1);
        }
        this.state = 171;
        this.match(PvsLanguageParser.K_ENDASSUMING);
    } catch (re) {
    	if(re instanceof antlr4.error.RecognitionException) {
	        localctx.exception = re;
	        this._errHandler.reportError(this, re);
	        this._errHandler.recover(this, re);
	    } else {
	    	throw re;
	    }
    } finally {
        this.exitRule();
    }
    return localctx;
};

function AssumingElementContext(parser, parent, invokingState) {
	if(parent===undefined) {
	    parent = null;
	}
	if(invokingState===undefined || invokingState===null) {
		invokingState = -1;
	}
	antlr4.ParserRuleContext.call(this, parent, invokingState);
    this.parser = parser;
    this.ruleIndex = PvsLanguageParser.RULE_assumingElement;
    return this;
}

AssumingElementContext.prototype = Object.create(antlr4.ParserRuleContext.prototype);
AssumingElementContext.prototype.constructor = AssumingElementContext;

AssumingElementContext.prototype.importing = function() {
    return this.getTypedRuleContext(ImportingContext,0);
};

AssumingElementContext.prototype.assumption = function() {
    return this.getTypedRuleContext(AssumptionContext,0);
};

AssumingElementContext.prototype.theoryDeclaration = function() {
    return this.getTypedRuleContext(TheoryDeclarationContext,0);
};

AssumingElementContext.prototype.enterRule = function(listener) {
    if(listener instanceof PvsLanguageListener ) {
        listener.enterAssumingElement(this);
	}
};

AssumingElementContext.prototype.exitRule = function(listener) {
    if(listener instanceof PvsLanguageListener ) {
        listener.exitAssumingElement(this);
	}
};




PvsLanguageParser.AssumingElementContext = AssumingElementContext;

PvsLanguageParser.prototype.assumingElement = function() {

    var localctx = new AssumingElementContext(this, this._ctx, this.state);
    this.enterRule(localctx, 14, PvsLanguageParser.RULE_assumingElement);
    try {
        this.state = 176;
        var la_ = this._interp.adaptivePredict(this._input,8,this._ctx);
        switch(la_) {
        case 1:
            this.enterOuterAlt(localctx, 1);
            this.state = 173;
            this.importing();
            break;

        case 2:
            this.enterOuterAlt(localctx, 2);
            this.state = 174;
            this.assumption();
            break;

        case 3:
            this.enterOuterAlt(localctx, 3);
            this.state = 175;
            this.theoryDeclaration();
            break;

        }
    } catch (re) {
    	if(re instanceof antlr4.error.RecognitionException) {
	        localctx.exception = re;
	        this._errHandler.reportError(this, re);
	        this._errHandler.recover(this, re);
	    } else {
	    	throw re;
	    }
    } finally {
        this.exitRule();
    }
    return localctx;
};

function AssumptionContext(parser, parent, invokingState) {
	if(parent===undefined) {
	    parent = null;
	}
	if(invokingState===undefined || invokingState===null) {
		invokingState = -1;
	}
	antlr4.ParserRuleContext.call(this, parent, invokingState);
    this.parser = parser;
    this.ruleIndex = PvsLanguageParser.RULE_assumption;
    return this;
}

AssumptionContext.prototype = Object.create(antlr4.ParserRuleContext.prototype);
AssumptionContext.prototype.constructor = AssumptionContext;

AssumptionContext.prototype.identifiers = function() {
    return this.getTypedRuleContext(IdentifiersContext,0);
};

AssumptionContext.prototype.K_ASSUMPTION = function() {
    return this.getToken(PvsLanguageParser.K_ASSUMPTION, 0);
};

AssumptionContext.prototype.expr = function() {
    return this.getTypedRuleContext(ExprContext,0);
};

AssumptionContext.prototype.enterRule = function(listener) {
    if(listener instanceof PvsLanguageListener ) {
        listener.enterAssumption(this);
	}
};

AssumptionContext.prototype.exitRule = function(listener) {
    if(listener instanceof PvsLanguageListener ) {
        listener.exitAssumption(this);
	}
};




PvsLanguageParser.AssumptionContext = AssumptionContext;

PvsLanguageParser.prototype.assumption = function() {

    var localctx = new AssumptionContext(this, this._ctx, this.state);
    this.enterRule(localctx, 16, PvsLanguageParser.RULE_assumption);
    try {
        this.enterOuterAlt(localctx, 1);
        this.state = 178;
        this.identifiers();
        this.state = 179;
        this.match(PvsLanguageParser.T__3);
        this.state = 180;
        this.match(PvsLanguageParser.K_ASSUMPTION);
        this.state = 181;
        this.expr(0);
    } catch (re) {
    	if(re instanceof antlr4.error.RecognitionException) {
	        localctx.exception = re;
	        this._errHandler.reportError(this, re);
	        this._errHandler.recover(this, re);
	    } else {
	    	throw re;
	    }
    } finally {
        this.exitRule();
    }
    return localctx;
};

function TheoryPartContext(parser, parent, invokingState) {
	if(parent===undefined) {
	    parent = null;
	}
	if(invokingState===undefined || invokingState===null) {
		invokingState = -1;
	}
	antlr4.ParserRuleContext.call(this, parent, invokingState);
    this.parser = parser;
    this.ruleIndex = PvsLanguageParser.RULE_theoryPart;
    return this;
}

TheoryPartContext.prototype = Object.create(antlr4.ParserRuleContext.prototype);
TheoryPartContext.prototype.constructor = TheoryPartContext;

TheoryPartContext.prototype.theoryElement = function(i) {
    if(i===undefined) {
        i = null;
    }
    if(i===null) {
        return this.getTypedRuleContexts(TheoryElementContext);
    } else {
        return this.getTypedRuleContext(TheoryElementContext,i);
    }
};

TheoryPartContext.prototype.enterRule = function(listener) {
    if(listener instanceof PvsLanguageListener ) {
        listener.enterTheoryPart(this);
	}
};

TheoryPartContext.prototype.exitRule = function(listener) {
    if(listener instanceof PvsLanguageListener ) {
        listener.exitTheoryPart(this);
	}
};




PvsLanguageParser.TheoryPartContext = TheoryPartContext;

PvsLanguageParser.prototype.theoryPart = function() {

    var localctx = new TheoryPartContext(this, this._ctx, this.state);
    this.enterRule(localctx, 18, PvsLanguageParser.RULE_theoryPart);
    var _la = 0; // Token type
    try {
        this.enterOuterAlt(localctx, 1);
        this.state = 184; 
        this._errHandler.sync(this);
        _la = this._input.LA(1);
        do {
            this.state = 183;
            this.theoryElement();
            this.state = 186; 
            this._errHandler.sync(this);
            _la = this._input.LA(1);
        } while(((((_la - 21)) & ~0x1f) == 0 && ((1 << (_la - 21)) & ((1 << (PvsLanguageParser.T__20 - 21)) | (1 << (PvsLanguageParser.T__21 - 21)) | (1 << (PvsLanguageParser.T__22 - 21)) | (1 << (PvsLanguageParser.T__23 - 21)) | (1 << (PvsLanguageParser.T__24 - 21)) | (1 << (PvsLanguageParser.T__25 - 21)) | (1 << (PvsLanguageParser.T__26 - 21)) | (1 << (PvsLanguageParser.T__27 - 21)) | (1 << (PvsLanguageParser.T__28 - 21)) | (1 << (PvsLanguageParser.T__29 - 21)) | (1 << (PvsLanguageParser.K_IMPORTING - 21)))) !== 0) || ((((_la - 63)) & ~0x1f) == 0 && ((1 << (_la - 63)) & ((1 << (PvsLanguageParser.K_CONVERSION - 63)) | (1 << (PvsLanguageParser.K_CONVERSION_PLUS - 63)) | (1 << (PvsLanguageParser.K_CONVERSION_MINUS - 63)) | (1 << (PvsLanguageParser.O_IFF - 63)) | (1 << (PvsLanguageParser.O_IMPLIES - 63)) | (1 << (PvsLanguageParser.O_NOT - 63)) | (1 << (PvsLanguageParser.O_AND - 63)) | (1 << (PvsLanguageParser.O_OR - 63)) | (1 << (PvsLanguageParser.O_NOT_EQUAL - 63)) | (1 << (PvsLanguageParser.O_LE - 63)) | (1 << (PvsLanguageParser.O_GE - 63)) | (1 << (PvsLanguageParser.O_EQUAL - 63)) | (1 << (PvsLanguageParser.O_EXP - 63)) | (1 << (PvsLanguageParser.O_CONCAT - 63)) | (1 << (PvsLanguageParser.ID - 63)))) !== 0));
    } catch (re) {
    	if(re instanceof antlr4.error.RecognitionException) {
	        localctx.exception = re;
	        this._errHandler.reportError(this, re);
	        this._errHandler.recover(this, re);
	    } else {
	    	throw re;
	    }
    } finally {
        this.exitRule();
    }
    return localctx;
};

function TheoryElementContext(parser, parent, invokingState) {
	if(parent===undefined) {
	    parent = null;
	}
	if(invokingState===undefined || invokingState===null) {
		invokingState = -1;
	}
	antlr4.ParserRuleContext.call(this, parent, invokingState);
    this.parser = parser;
    this.ruleIndex = PvsLanguageParser.RULE_theoryElement;
    return this;
}

TheoryElementContext.prototype = Object.create(antlr4.ParserRuleContext.prototype);
TheoryElementContext.prototype.constructor = TheoryElementContext;

TheoryElementContext.prototype.importing = function() {
    return this.getTypedRuleContext(ImportingContext,0);
};

TheoryElementContext.prototype.theoryDeclaration = function() {
    return this.getTypedRuleContext(TheoryDeclarationContext,0);
};

TheoryElementContext.prototype.enterRule = function(listener) {
    if(listener instanceof PvsLanguageListener ) {
        listener.enterTheoryElement(this);
	}
};

TheoryElementContext.prototype.exitRule = function(listener) {
    if(listener instanceof PvsLanguageListener ) {
        listener.exitTheoryElement(this);
	}
};




PvsLanguageParser.TheoryElementContext = TheoryElementContext;

PvsLanguageParser.prototype.theoryElement = function() {

    var localctx = new TheoryElementContext(this, this._ctx, this.state);
    this.enterRule(localctx, 20, PvsLanguageParser.RULE_theoryElement);
    var _la = 0; // Token type
    try {
        this.state = 193;
        switch(this._input.LA(1)) {
        case PvsLanguageParser.K_IMPORTING:
            this.enterOuterAlt(localctx, 1);
            this.state = 188;
            this.importing();
            break;
        case PvsLanguageParser.T__20:
        case PvsLanguageParser.T__21:
        case PvsLanguageParser.T__22:
        case PvsLanguageParser.T__23:
        case PvsLanguageParser.T__24:
        case PvsLanguageParser.T__25:
        case PvsLanguageParser.T__26:
        case PvsLanguageParser.T__27:
        case PvsLanguageParser.T__28:
        case PvsLanguageParser.T__29:
        case PvsLanguageParser.K_CONVERSION:
        case PvsLanguageParser.K_CONVERSION_PLUS:
        case PvsLanguageParser.K_CONVERSION_MINUS:
        case PvsLanguageParser.O_IFF:
        case PvsLanguageParser.O_IMPLIES:
        case PvsLanguageParser.O_NOT:
        case PvsLanguageParser.O_AND:
        case PvsLanguageParser.O_OR:
        case PvsLanguageParser.O_NOT_EQUAL:
        case PvsLanguageParser.O_LE:
        case PvsLanguageParser.O_GE:
        case PvsLanguageParser.O_EQUAL:
        case PvsLanguageParser.O_EXP:
        case PvsLanguageParser.O_CONCAT:
        case PvsLanguageParser.ID:
            this.enterOuterAlt(localctx, 2);
            this.state = 189;
            this.theoryDeclaration();
            this.state = 191;
            _la = this._input.LA(1);
            if(_la===PvsLanguageParser.T__6) {
                this.state = 190;
                this.match(PvsLanguageParser.T__6);
            }

            break;
        default:
            throw new antlr4.error.NoViableAltException(this);
        }
    } catch (re) {
    	if(re instanceof antlr4.error.RecognitionException) {
	        localctx.exception = re;
	        this._errHandler.reportError(this, re);
	        this._errHandler.recover(this, re);
	    } else {
	    	throw re;
	    }
    } finally {
        this.exitRule();
    }
    return localctx;
};

function TheoryDeclarationContext(parser, parent, invokingState) {
	if(parent===undefined) {
	    parent = null;
	}
	if(invokingState===undefined || invokingState===null) {
		invokingState = -1;
	}
	antlr4.ParserRuleContext.call(this, parent, invokingState);
    this.parser = parser;
    this.ruleIndex = PvsLanguageParser.RULE_theoryDeclaration;
    return this;
}

TheoryDeclarationContext.prototype = Object.create(antlr4.ParserRuleContext.prototype);
TheoryDeclarationContext.prototype.constructor = TheoryDeclarationContext;

TheoryDeclarationContext.prototype.typeDeclaration = function() {
    return this.getTypedRuleContext(TypeDeclarationContext,0);
};

TheoryDeclarationContext.prototype.formulaDeclaration = function() {
    return this.getTypedRuleContext(FormulaDeclarationContext,0);
};

TheoryDeclarationContext.prototype.constantDeclaration = function() {
    return this.getTypedRuleContext(ConstantDeclarationContext,0);
};

TheoryDeclarationContext.prototype.functionDeclaration = function() {
    return this.getTypedRuleContext(FunctionDeclarationContext,0);
};

TheoryDeclarationContext.prototype.recursiveDeclaration = function() {
    return this.getTypedRuleContext(RecursiveDeclarationContext,0);
};

TheoryDeclarationContext.prototype.conversionDeclaration = function() {
    return this.getTypedRuleContext(ConversionDeclarationContext,0);
};

TheoryDeclarationContext.prototype.varDeclaration = function() {
    return this.getTypedRuleContext(VarDeclarationContext,0);
};

TheoryDeclarationContext.prototype.enterRule = function(listener) {
    if(listener instanceof PvsLanguageListener ) {
        listener.enterTheoryDeclaration(this);
	}
};

TheoryDeclarationContext.prototype.exitRule = function(listener) {
    if(listener instanceof PvsLanguageListener ) {
        listener.exitTheoryDeclaration(this);
	}
};




PvsLanguageParser.TheoryDeclarationContext = TheoryDeclarationContext;

PvsLanguageParser.prototype.theoryDeclaration = function() {

    var localctx = new TheoryDeclarationContext(this, this._ctx, this.state);
    this.enterRule(localctx, 22, PvsLanguageParser.RULE_theoryDeclaration);
    try {
        this.state = 202;
        var la_ = this._interp.adaptivePredict(this._input,12,this._ctx);
        switch(la_) {
        case 1:
            this.enterOuterAlt(localctx, 1);
            this.state = 195;
            this.typeDeclaration();
            break;

        case 2:
            this.enterOuterAlt(localctx, 2);
            this.state = 196;
            this.formulaDeclaration();
            break;

        case 3:
            this.enterOuterAlt(localctx, 3);
            this.state = 197;
            this.constantDeclaration();
            break;

        case 4:
            this.enterOuterAlt(localctx, 4);
            this.state = 198;
            this.functionDeclaration();
            break;

        case 5:
            this.enterOuterAlt(localctx, 5);
            this.state = 199;
            this.recursiveDeclaration();
            break;

        case 6:
            this.enterOuterAlt(localctx, 6);
            this.state = 200;
            this.conversionDeclaration();
            break;

        case 7:
            this.enterOuterAlt(localctx, 7);
            this.state = 201;
            this.varDeclaration();
            break;

        }
    } catch (re) {
    	if(re instanceof antlr4.error.RecognitionException) {
	        localctx.exception = re;
	        this._errHandler.reportError(this, re);
	        this._errHandler.recover(this, re);
	    } else {
	    	throw re;
	    }
    } finally {
        this.exitRule();
    }
    return localctx;
};

function TypeDeclarationContext(parser, parent, invokingState) {
	if(parent===undefined) {
	    parent = null;
	}
	if(invokingState===undefined || invokingState===null) {
		invokingState = -1;
	}
	antlr4.ParserRuleContext.call(this, parent, invokingState);
    this.parser = parser;
    this.ruleIndex = PvsLanguageParser.RULE_typeDeclaration;
    return this;
}

TypeDeclarationContext.prototype = Object.create(antlr4.ParserRuleContext.prototype);
TypeDeclarationContext.prototype.constructor = TypeDeclarationContext;

TypeDeclarationContext.prototype.identifiers = function() {
    return this.getTypedRuleContext(IdentifiersContext,0);
};

TypeDeclarationContext.prototype.K_TYPE = function() {
    return this.getToken(PvsLanguageParser.K_TYPE, 0);
};

TypeDeclarationContext.prototype.K_NONEMPTY_TYPE = function() {
    return this.getToken(PvsLanguageParser.K_NONEMPTY_TYPE, 0);
};

TypeDeclarationContext.prototype.K_TYPE_PLUS = function() {
    return this.getToken(PvsLanguageParser.K_TYPE_PLUS, 0);
};

TypeDeclarationContext.prototype.arguments = function(i) {
    if(i===undefined) {
        i = null;
    }
    if(i===null) {
        return this.getTypedRuleContexts(ArgumentsContext);
    } else {
        return this.getTypedRuleContext(ArgumentsContext,i);
    }
};

TypeDeclarationContext.prototype.typeDefinition = function() {
    return this.getTypedRuleContext(TypeDefinitionContext,0);
};

TypeDeclarationContext.prototype.enterRule = function(listener) {
    if(listener instanceof PvsLanguageListener ) {
        listener.enterTypeDeclaration(this);
	}
};

TypeDeclarationContext.prototype.exitRule = function(listener) {
    if(listener instanceof PvsLanguageListener ) {
        listener.exitTypeDeclaration(this);
	}
};




PvsLanguageParser.TypeDeclarationContext = TypeDeclarationContext;

PvsLanguageParser.prototype.typeDeclaration = function() {

    var localctx = new TypeDeclarationContext(this, this._ctx, this.state);
    this.enterRule(localctx, 24, PvsLanguageParser.RULE_typeDeclaration);
    var _la = 0; // Token type
    try {
        this.enterOuterAlt(localctx, 1);
        this.state = 204;
        this.identifiers();
        this.state = 208;
        this._errHandler.sync(this);
        _la = this._input.LA(1);
        while(_la===PvsLanguageParser.T__4) {
            this.state = 205;
            this.arguments();
            this.state = 210;
            this._errHandler.sync(this);
            _la = this._input.LA(1);
        }
        this.state = 211;
        this.match(PvsLanguageParser.T__3);
        this.state = 212;
        _la = this._input.LA(1);
        if(!(((((_la - 36)) & ~0x1f) == 0 && ((1 << (_la - 36)) & ((1 << (PvsLanguageParser.K_TYPE - 36)) | (1 << (PvsLanguageParser.K_NONEMPTY_TYPE - 36)) | (1 << (PvsLanguageParser.K_TYPE_PLUS - 36)))) !== 0))) {
        this._errHandler.recoverInline(this);
        }
        else {
            this.consume();
        }
        this.state = 214;
        var la_ = this._interp.adaptivePredict(this._input,14,this._ctx);
        if(la_===1) {
            this.state = 213;
            this.typeDefinition();

        }
    } catch (re) {
    	if(re instanceof antlr4.error.RecognitionException) {
	        localctx.exception = re;
	        this._errHandler.reportError(this, re);
	        this._errHandler.recover(this, re);
	    } else {
	    	throw re;
	    }
    } finally {
        this.exitRule();
    }
    return localctx;
};

function TypeDefinitionContext(parser, parent, invokingState) {
	if(parent===undefined) {
	    parent = null;
	}
	if(invokingState===undefined || invokingState===null) {
		invokingState = -1;
	}
	antlr4.ParserRuleContext.call(this, parent, invokingState);
    this.parser = parser;
    this.ruleIndex = PvsLanguageParser.RULE_typeDefinition;
    return this;
}

TypeDefinitionContext.prototype = Object.create(antlr4.ParserRuleContext.prototype);
TypeDefinitionContext.prototype.constructor = TypeDefinitionContext;

TypeDefinitionContext.prototype.typeExpression = function() {
    return this.getTypedRuleContext(TypeExpressionContext,0);
};

TypeDefinitionContext.prototype.K_FROM = function() {
    return this.getToken(PvsLanguageParser.K_FROM, 0);
};

TypeDefinitionContext.prototype.K_CONTAINING = function() {
    return this.getToken(PvsLanguageParser.K_CONTAINING, 0);
};

TypeDefinitionContext.prototype.expr = function() {
    return this.getTypedRuleContext(ExprContext,0);
};

TypeDefinitionContext.prototype.enterRule = function(listener) {
    if(listener instanceof PvsLanguageListener ) {
        listener.enterTypeDefinition(this);
	}
};

TypeDefinitionContext.prototype.exitRule = function(listener) {
    if(listener instanceof PvsLanguageListener ) {
        listener.exitTypeDefinition(this);
	}
};




PvsLanguageParser.TypeDefinitionContext = TypeDefinitionContext;

PvsLanguageParser.prototype.typeDefinition = function() {

    var localctx = new TypeDefinitionContext(this, this._ctx, this.state);
    this.enterRule(localctx, 26, PvsLanguageParser.RULE_typeDefinition);
    var _la = 0; // Token type
    try {
        this.enterOuterAlt(localctx, 1);
        this.state = 216;
        _la = this._input.LA(1);
        if(!(_la===PvsLanguageParser.K_FROM || _la===PvsLanguageParser.O_EQUAL)) {
        this._errHandler.recoverInline(this);
        }
        else {
            this.consume();
        }
        this.state = 217;
        this.typeExpression();
        this.state = 220;
        _la = this._input.LA(1);
        if(_la===PvsLanguageParser.K_CONTAINING) {
            this.state = 218;
            this.match(PvsLanguageParser.K_CONTAINING);
            this.state = 219;
            this.expr(0);
        }

    } catch (re) {
    	if(re instanceof antlr4.error.RecognitionException) {
	        localctx.exception = re;
	        this._errHandler.reportError(this, re);
	        this._errHandler.recover(this, re);
	    } else {
	    	throw re;
	    }
    } finally {
        this.exitRule();
    }
    return localctx;
};

function FormulaDeclarationContext(parser, parent, invokingState) {
	if(parent===undefined) {
	    parent = null;
	}
	if(invokingState===undefined || invokingState===null) {
		invokingState = -1;
	}
	antlr4.ParserRuleContext.call(this, parent, invokingState);
    this.parser = parser;
    this.ruleIndex = PvsLanguageParser.RULE_formulaDeclaration;
    return this;
}

FormulaDeclarationContext.prototype = Object.create(antlr4.ParserRuleContext.prototype);
FormulaDeclarationContext.prototype.constructor = FormulaDeclarationContext;

FormulaDeclarationContext.prototype.identifiers = function() {
    return this.getTypedRuleContext(IdentifiersContext,0);
};

FormulaDeclarationContext.prototype.K_FORMULA = function() {
    return this.getToken(PvsLanguageParser.K_FORMULA, 0);
};

FormulaDeclarationContext.prototype.expr = function() {
    return this.getTypedRuleContext(ExprContext,0);
};

FormulaDeclarationContext.prototype.enterRule = function(listener) {
    if(listener instanceof PvsLanguageListener ) {
        listener.enterFormulaDeclaration(this);
	}
};

FormulaDeclarationContext.prototype.exitRule = function(listener) {
    if(listener instanceof PvsLanguageListener ) {
        listener.exitFormulaDeclaration(this);
	}
};




PvsLanguageParser.FormulaDeclarationContext = FormulaDeclarationContext;

PvsLanguageParser.prototype.formulaDeclaration = function() {

    var localctx = new FormulaDeclarationContext(this, this._ctx, this.state);
    this.enterRule(localctx, 28, PvsLanguageParser.RULE_formulaDeclaration);
    try {
        this.enterOuterAlt(localctx, 1);
        this.state = 222;
        this.identifiers();
        this.state = 223;
        this.match(PvsLanguageParser.T__3);
        this.state = 224;
        this.match(PvsLanguageParser.K_FORMULA);
        this.state = 225;
        this.expr(0);
    } catch (re) {
    	if(re instanceof antlr4.error.RecognitionException) {
	        localctx.exception = re;
	        this._errHandler.reportError(this, re);
	        this._errHandler.recover(this, re);
	    } else {
	    	throw re;
	    }
    } finally {
        this.exitRule();
    }
    return localctx;
};

function ConstantDeclarationContext(parser, parent, invokingState) {
	if(parent===undefined) {
	    parent = null;
	}
	if(invokingState===undefined || invokingState===null) {
		invokingState = -1;
	}
	antlr4.ParserRuleContext.call(this, parent, invokingState);
    this.parser = parser;
    this.ruleIndex = PvsLanguageParser.RULE_constantDeclaration;
    return this;
}

ConstantDeclarationContext.prototype = Object.create(antlr4.ParserRuleContext.prototype);
ConstantDeclarationContext.prototype.constructor = ConstantDeclarationContext;

ConstantDeclarationContext.prototype.identifierOrOperators = function() {
    return this.getTypedRuleContext(IdentifierOrOperatorsContext,0);
};

ConstantDeclarationContext.prototype.typeExpression = function() {
    return this.getTypedRuleContext(TypeExpressionContext,0);
};

ConstantDeclarationContext.prototype.expr = function() {
    return this.getTypedRuleContext(ExprContext,0);
};

ConstantDeclarationContext.prototype.enterRule = function(listener) {
    if(listener instanceof PvsLanguageListener ) {
        listener.enterConstantDeclaration(this);
	}
};

ConstantDeclarationContext.prototype.exitRule = function(listener) {
    if(listener instanceof PvsLanguageListener ) {
        listener.exitConstantDeclaration(this);
	}
};




PvsLanguageParser.ConstantDeclarationContext = ConstantDeclarationContext;

PvsLanguageParser.prototype.constantDeclaration = function() {

    var localctx = new ConstantDeclarationContext(this, this._ctx, this.state);
    this.enterRule(localctx, 30, PvsLanguageParser.RULE_constantDeclaration);
    try {
        this.enterOuterAlt(localctx, 1);
        this.state = 227;
        this.identifierOrOperators();
        this.state = 228;
        this.match(PvsLanguageParser.T__3);
        this.state = 229;
        this.typeExpression();
        this.state = 232;
        var la_ = this._interp.adaptivePredict(this._input,16,this._ctx);
        if(la_===1) {
            this.state = 230;
            this.match(PvsLanguageParser.O_EQUAL);
            this.state = 231;
            this.expr(0);

        }
    } catch (re) {
    	if(re instanceof antlr4.error.RecognitionException) {
	        localctx.exception = re;
	        this._errHandler.reportError(this, re);
	        this._errHandler.recover(this, re);
	    } else {
	    	throw re;
	    }
    } finally {
        this.exitRule();
    }
    return localctx;
};

function FunctionDeclarationContext(parser, parent, invokingState) {
	if(parent===undefined) {
	    parent = null;
	}
	if(invokingState===undefined || invokingState===null) {
		invokingState = -1;
	}
	antlr4.ParserRuleContext.call(this, parent, invokingState);
    this.parser = parser;
    this.ruleIndex = PvsLanguageParser.RULE_functionDeclaration;
    return this;
}

FunctionDeclarationContext.prototype = Object.create(antlr4.ParserRuleContext.prototype);
FunctionDeclarationContext.prototype.constructor = FunctionDeclarationContext;

FunctionDeclarationContext.prototype.identifierOrOperator = function() {
    return this.getTypedRuleContext(IdentifierOrOperatorContext,0);
};

FunctionDeclarationContext.prototype.typeExpression = function() {
    return this.getTypedRuleContext(TypeExpressionContext,0);
};

FunctionDeclarationContext.prototype.arguments = function(i) {
    if(i===undefined) {
        i = null;
    }
    if(i===null) {
        return this.getTypedRuleContexts(ArgumentsContext);
    } else {
        return this.getTypedRuleContext(ArgumentsContext,i);
    }
};

FunctionDeclarationContext.prototype.expr = function() {
    return this.getTypedRuleContext(ExprContext,0);
};

FunctionDeclarationContext.prototype.K_MACRO = function() {
    return this.getToken(PvsLanguageParser.K_MACRO, 0);
};

FunctionDeclarationContext.prototype.K_INDUCTIVE = function() {
    return this.getToken(PvsLanguageParser.K_INDUCTIVE, 0);
};

FunctionDeclarationContext.prototype.enterRule = function(listener) {
    if(listener instanceof PvsLanguageListener ) {
        listener.enterFunctionDeclaration(this);
	}
};

FunctionDeclarationContext.prototype.exitRule = function(listener) {
    if(listener instanceof PvsLanguageListener ) {
        listener.exitFunctionDeclaration(this);
	}
};




PvsLanguageParser.FunctionDeclarationContext = FunctionDeclarationContext;

PvsLanguageParser.prototype.functionDeclaration = function() {

    var localctx = new FunctionDeclarationContext(this, this._ctx, this.state);
    this.enterRule(localctx, 32, PvsLanguageParser.RULE_functionDeclaration);
    var _la = 0; // Token type
    try {
        this.enterOuterAlt(localctx, 1);
        this.state = 234;
        this.identifierOrOperator();
        this.state = 238;
        this._errHandler.sync(this);
        _la = this._input.LA(1);
        while(_la===PvsLanguageParser.T__4) {
            this.state = 235;
            this.arguments();
            this.state = 240;
            this._errHandler.sync(this);
            _la = this._input.LA(1);
        }
        this.state = 241;
        this.match(PvsLanguageParser.T__3);
        this.state = 243;
        _la = this._input.LA(1);
        if(_la===PvsLanguageParser.K_MACRO || _la===PvsLanguageParser.K_INDUCTIVE) {
            this.state = 242;
            _la = this._input.LA(1);
            if(!(_la===PvsLanguageParser.K_MACRO || _la===PvsLanguageParser.K_INDUCTIVE)) {
            this._errHandler.recoverInline(this);
            }
            else {
                this.consume();
            }
        }

        this.state = 245;
        this.typeExpression();
        this.state = 248;
        var la_ = this._interp.adaptivePredict(this._input,19,this._ctx);
        if(la_===1) {
            this.state = 246;
            this.match(PvsLanguageParser.O_EQUAL);
            this.state = 247;
            this.expr(0);

        }
    } catch (re) {
    	if(re instanceof antlr4.error.RecognitionException) {
	        localctx.exception = re;
	        this._errHandler.reportError(this, re);
	        this._errHandler.recover(this, re);
	    } else {
	    	throw re;
	    }
    } finally {
        this.exitRule();
    }
    return localctx;
};

function RecursiveDeclarationContext(parser, parent, invokingState) {
	if(parent===undefined) {
	    parent = null;
	}
	if(invokingState===undefined || invokingState===null) {
		invokingState = -1;
	}
	antlr4.ParserRuleContext.call(this, parent, invokingState);
    this.parser = parser;
    this.ruleIndex = PvsLanguageParser.RULE_recursiveDeclaration;
    return this;
}

RecursiveDeclarationContext.prototype = Object.create(antlr4.ParserRuleContext.prototype);
RecursiveDeclarationContext.prototype.constructor = RecursiveDeclarationContext;

RecursiveDeclarationContext.prototype.identifierOrOperator = function() {
    return this.getTypedRuleContext(IdentifierOrOperatorContext,0);
};

RecursiveDeclarationContext.prototype.K_RECURSIVE = function() {
    return this.getToken(PvsLanguageParser.K_RECURSIVE, 0);
};

RecursiveDeclarationContext.prototype.typeExpression = function() {
    return this.getTypedRuleContext(TypeExpressionContext,0);
};

RecursiveDeclarationContext.prototype.expr = function() {
    return this.getTypedRuleContext(ExprContext,0);
};

RecursiveDeclarationContext.prototype.measureExpression = function() {
    return this.getTypedRuleContext(MeasureExpressionContext,0);
};

RecursiveDeclarationContext.prototype.arguments = function(i) {
    if(i===undefined) {
        i = null;
    }
    if(i===null) {
        return this.getTypedRuleContexts(ArgumentsContext);
    } else {
        return this.getTypedRuleContext(ArgumentsContext,i);
    }
};

RecursiveDeclarationContext.prototype.enterRule = function(listener) {
    if(listener instanceof PvsLanguageListener ) {
        listener.enterRecursiveDeclaration(this);
	}
};

RecursiveDeclarationContext.prototype.exitRule = function(listener) {
    if(listener instanceof PvsLanguageListener ) {
        listener.exitRecursiveDeclaration(this);
	}
};




PvsLanguageParser.RecursiveDeclarationContext = RecursiveDeclarationContext;

PvsLanguageParser.prototype.recursiveDeclaration = function() {

    var localctx = new RecursiveDeclarationContext(this, this._ctx, this.state);
    this.enterRule(localctx, 34, PvsLanguageParser.RULE_recursiveDeclaration);
    var _la = 0; // Token type
    try {
        this.enterOuterAlt(localctx, 1);
        this.state = 250;
        this.identifierOrOperator();
        this.state = 254;
        this._errHandler.sync(this);
        _la = this._input.LA(1);
        while(_la===PvsLanguageParser.T__4) {
            this.state = 251;
            this.arguments();
            this.state = 256;
            this._errHandler.sync(this);
            _la = this._input.LA(1);
        }
        this.state = 257;
        this.match(PvsLanguageParser.T__3);
        this.state = 258;
        this.match(PvsLanguageParser.K_RECURSIVE);
        this.state = 259;
        this.typeExpression();
        this.state = 260;
        this.match(PvsLanguageParser.O_EQUAL);
        this.state = 261;
        this.expr(0);
        this.state = 262;
        this.measureExpression();
    } catch (re) {
    	if(re instanceof antlr4.error.RecognitionException) {
	        localctx.exception = re;
	        this._errHandler.reportError(this, re);
	        this._errHandler.recover(this, re);
	    } else {
	    	throw re;
	    }
    } finally {
        this.exitRule();
    }
    return localctx;
};

function ConversionDeclarationContext(parser, parent, invokingState) {
	if(parent===undefined) {
	    parent = null;
	}
	if(invokingState===undefined || invokingState===null) {
		invokingState = -1;
	}
	antlr4.ParserRuleContext.call(this, parent, invokingState);
    this.parser = parser;
    this.ruleIndex = PvsLanguageParser.RULE_conversionDeclaration;
    return this;
}

ConversionDeclarationContext.prototype = Object.create(antlr4.ParserRuleContext.prototype);
ConversionDeclarationContext.prototype.constructor = ConversionDeclarationContext;

ConversionDeclarationContext.prototype.name = function(i) {
    if(i===undefined) {
        i = null;
    }
    if(i===null) {
        return this.getTypedRuleContexts(NameContext);
    } else {
        return this.getTypedRuleContext(NameContext,i);
    }
};

ConversionDeclarationContext.prototype.K_CONVERSION = function() {
    return this.getToken(PvsLanguageParser.K_CONVERSION, 0);
};

ConversionDeclarationContext.prototype.K_CONVERSION_PLUS = function() {
    return this.getToken(PvsLanguageParser.K_CONVERSION_PLUS, 0);
};

ConversionDeclarationContext.prototype.K_CONVERSION_MINUS = function() {
    return this.getToken(PvsLanguageParser.K_CONVERSION_MINUS, 0);
};

ConversionDeclarationContext.prototype.typeExpression = function(i) {
    if(i===undefined) {
        i = null;
    }
    if(i===null) {
        return this.getTypedRuleContexts(TypeExpressionContext);
    } else {
        return this.getTypedRuleContext(TypeExpressionContext,i);
    }
};

ConversionDeclarationContext.prototype.enterRule = function(listener) {
    if(listener instanceof PvsLanguageListener ) {
        listener.enterConversionDeclaration(this);
	}
};

ConversionDeclarationContext.prototype.exitRule = function(listener) {
    if(listener instanceof PvsLanguageListener ) {
        listener.exitConversionDeclaration(this);
	}
};




PvsLanguageParser.ConversionDeclarationContext = ConversionDeclarationContext;

PvsLanguageParser.prototype.conversionDeclaration = function() {

    var localctx = new ConversionDeclarationContext(this, this._ctx, this.state);
    this.enterRule(localctx, 36, PvsLanguageParser.RULE_conversionDeclaration);
    var _la = 0; // Token type
    try {
        this.enterOuterAlt(localctx, 1);
        this.state = 264;
        _la = this._input.LA(1);
        if(!(((((_la - 63)) & ~0x1f) == 0 && ((1 << (_la - 63)) & ((1 << (PvsLanguageParser.K_CONVERSION - 63)) | (1 << (PvsLanguageParser.K_CONVERSION_PLUS - 63)) | (1 << (PvsLanguageParser.K_CONVERSION_MINUS - 63)))) !== 0))) {
        this._errHandler.recoverInline(this);
        }
        else {
            this.consume();
        }
        this.state = 265;
        this.name();
        this.state = 268;
        _la = this._input.LA(1);
        if(_la===PvsLanguageParser.T__3) {
            this.state = 266;
            this.match(PvsLanguageParser.T__3);
            this.state = 267;
            this.typeExpression();
        }

        this.state = 277;
        this._errHandler.sync(this);
        _la = this._input.LA(1);
        while(_la===PvsLanguageParser.T__1) {
            this.state = 270;
            this.match(PvsLanguageParser.T__1);
            this.state = 271;
            this.name();

            this.state = 272;
            this.match(PvsLanguageParser.T__3);
            this.state = 273;
            this.typeExpression();
            this.state = 279;
            this._errHandler.sync(this);
            _la = this._input.LA(1);
        }
    } catch (re) {
    	if(re instanceof antlr4.error.RecognitionException) {
	        localctx.exception = re;
	        this._errHandler.reportError(this, re);
	        this._errHandler.recover(this, re);
	    } else {
	    	throw re;
	    }
    } finally {
        this.exitRule();
    }
    return localctx;
};

function VarDeclarationContext(parser, parent, invokingState) {
	if(parent===undefined) {
	    parent = null;
	}
	if(invokingState===undefined || invokingState===null) {
		invokingState = -1;
	}
	antlr4.ParserRuleContext.call(this, parent, invokingState);
    this.parser = parser;
    this.ruleIndex = PvsLanguageParser.RULE_varDeclaration;
    return this;
}

VarDeclarationContext.prototype = Object.create(antlr4.ParserRuleContext.prototype);
VarDeclarationContext.prototype.constructor = VarDeclarationContext;

VarDeclarationContext.prototype.identifierOrOperators = function() {
    return this.getTypedRuleContext(IdentifierOrOperatorsContext,0);
};

VarDeclarationContext.prototype.K_VAR = function() {
    return this.getToken(PvsLanguageParser.K_VAR, 0);
};

VarDeclarationContext.prototype.typeExpression = function() {
    return this.getTypedRuleContext(TypeExpressionContext,0);
};

VarDeclarationContext.prototype.enterRule = function(listener) {
    if(listener instanceof PvsLanguageListener ) {
        listener.enterVarDeclaration(this);
	}
};

VarDeclarationContext.prototype.exitRule = function(listener) {
    if(listener instanceof PvsLanguageListener ) {
        listener.exitVarDeclaration(this);
	}
};




PvsLanguageParser.VarDeclarationContext = VarDeclarationContext;

PvsLanguageParser.prototype.varDeclaration = function() {

    var localctx = new VarDeclarationContext(this, this._ctx, this.state);
    this.enterRule(localctx, 38, PvsLanguageParser.RULE_varDeclaration);
    try {
        this.enterOuterAlt(localctx, 1);
        this.state = 280;
        this.identifierOrOperators();
        this.state = 281;
        this.match(PvsLanguageParser.T__3);
        this.state = 282;
        this.match(PvsLanguageParser.K_VAR);
        this.state = 283;
        this.typeExpression();
    } catch (re) {
    	if(re instanceof antlr4.error.RecognitionException) {
	        localctx.exception = re;
	        this._errHandler.reportError(this, re);
	        this._errHandler.recover(this, re);
	    } else {
	    	throw re;
	    }
    } finally {
        this.exitRule();
    }
    return localctx;
};

function ArgumentsContext(parser, parent, invokingState) {
	if(parent===undefined) {
	    parent = null;
	}
	if(invokingState===undefined || invokingState===null) {
		invokingState = -1;
	}
	antlr4.ParserRuleContext.call(this, parent, invokingState);
    this.parser = parser;
    this.ruleIndex = PvsLanguageParser.RULE_arguments;
    return this;
}

ArgumentsContext.prototype = Object.create(antlr4.ParserRuleContext.prototype);
ArgumentsContext.prototype.constructor = ArgumentsContext;

ArgumentsContext.prototype.identifiers = function(i) {
    if(i===undefined) {
        i = null;
    }
    if(i===null) {
        return this.getTypedRuleContexts(IdentifiersContext);
    } else {
        return this.getTypedRuleContext(IdentifiersContext,i);
    }
};

ArgumentsContext.prototype.expr = function(i) {
    if(i===undefined) {
        i = null;
    }
    if(i===null) {
        return this.getTypedRuleContexts(ExprContext);
    } else {
        return this.getTypedRuleContext(ExprContext,i);
    }
};

ArgumentsContext.prototype.subtype = function(i) {
    if(i===undefined) {
        i = null;
    }
    if(i===null) {
        return this.getTypedRuleContexts(SubtypeContext);
    } else {
        return this.getTypedRuleContext(SubtypeContext,i);
    }
};

ArgumentsContext.prototype.enterRule = function(listener) {
    if(listener instanceof PvsLanguageListener ) {
        listener.enterArguments(this);
	}
};

ArgumentsContext.prototype.exitRule = function(listener) {
    if(listener instanceof PvsLanguageListener ) {
        listener.exitArguments(this);
	}
};




PvsLanguageParser.ArgumentsContext = ArgumentsContext;

PvsLanguageParser.prototype.arguments = function() {

    var localctx = new ArgumentsContext(this, this._ctx, this.state);
    this.enterRule(localctx, 40, PvsLanguageParser.RULE_arguments);
    var _la = 0; // Token type
    try {
        this.state = 315;
        var la_ = this._interp.adaptivePredict(this._input,27,this._ctx);
        switch(la_) {
        case 1:
            this.enterOuterAlt(localctx, 1);
            this.state = 285;
            this.match(PvsLanguageParser.T__4);
            this.state = 286;
            this.identifiers();
            this.state = 289;
            _la = this._input.LA(1);
            if(_la===PvsLanguageParser.T__3) {
                this.state = 287;
                this.match(PvsLanguageParser.T__3);
                this.state = 288;
                this.expr(0);
            }

            this.state = 299;
            this._errHandler.sync(this);
            _la = this._input.LA(1);
            while(_la===PvsLanguageParser.T__1) {
                this.state = 291;
                this.match(PvsLanguageParser.T__1);
                this.state = 292;
                this.identifiers();
                this.state = 295;
                _la = this._input.LA(1);
                if(_la===PvsLanguageParser.T__3) {
                    this.state = 293;
                    this.match(PvsLanguageParser.T__3);
                    this.state = 294;
                    this.expr(0);
                }

                this.state = 301;
                this._errHandler.sync(this);
                _la = this._input.LA(1);
            }
            this.state = 302;
            this.match(PvsLanguageParser.T__5);
            break;

        case 2:
            this.enterOuterAlt(localctx, 2);
            this.state = 304;
            this.match(PvsLanguageParser.T__4);
            this.state = 305;
            this.subtype();
            this.state = 310;
            this._errHandler.sync(this);
            _la = this._input.LA(1);
            while(_la===PvsLanguageParser.T__1) {
                this.state = 306;
                this.match(PvsLanguageParser.T__1);
                this.state = 307;
                this.subtype();
                this.state = 312;
                this._errHandler.sync(this);
                _la = this._input.LA(1);
            }
            this.state = 313;
            this.match(PvsLanguageParser.T__5);
            break;

        }
    } catch (re) {
    	if(re instanceof antlr4.error.RecognitionException) {
	        localctx.exception = re;
	        this._errHandler.reportError(this, re);
	        this._errHandler.recover(this, re);
	    } else {
	    	throw re;
	    }
    } finally {
        this.exitRule();
    }
    return localctx;
};

function TypeExpressionContext(parser, parent, invokingState) {
	if(parent===undefined) {
	    parent = null;
	}
	if(invokingState===undefined || invokingState===null) {
		invokingState = -1;
	}
	antlr4.ParserRuleContext.call(this, parent, invokingState);
    this.parser = parser;
    this.ruleIndex = PvsLanguageParser.RULE_typeExpression;
    return this;
}

TypeExpressionContext.prototype = Object.create(antlr4.ParserRuleContext.prototype);
TypeExpressionContext.prototype.constructor = TypeExpressionContext;

TypeExpressionContext.prototype.enumerationType = function() {
    return this.getTypedRuleContext(EnumerationTypeContext,0);
};

TypeExpressionContext.prototype.recordType = function() {
    return this.getTypedRuleContext(RecordTypeContext,0);
};

TypeExpressionContext.prototype.tupleType = function() {
    return this.getTypedRuleContext(TupleTypeContext,0);
};

TypeExpressionContext.prototype.functionType = function() {
    return this.getTypedRuleContext(FunctionTypeContext,0);
};

TypeExpressionContext.prototype.bindingDeclaration = function() {
    return this.getTypedRuleContext(BindingDeclarationContext,0);
};

TypeExpressionContext.prototype.subtype = function() {
    return this.getTypedRuleContext(SubtypeContext,0);
};

TypeExpressionContext.prototype.name = function() {
    return this.getTypedRuleContext(NameContext,0);
};

TypeExpressionContext.prototype.enterRule = function(listener) {
    if(listener instanceof PvsLanguageListener ) {
        listener.enterTypeExpression(this);
	}
};

TypeExpressionContext.prototype.exitRule = function(listener) {
    if(listener instanceof PvsLanguageListener ) {
        listener.exitTypeExpression(this);
	}
};




PvsLanguageParser.TypeExpressionContext = TypeExpressionContext;

PvsLanguageParser.prototype.typeExpression = function() {

    var localctx = new TypeExpressionContext(this, this._ctx, this.state);
    this.enterRule(localctx, 42, PvsLanguageParser.RULE_typeExpression);
    try {
        this.state = 324;
        var la_ = this._interp.adaptivePredict(this._input,28,this._ctx);
        switch(la_) {
        case 1:
            this.enterOuterAlt(localctx, 1);
            this.state = 317;
            this.enumerationType();
            break;

        case 2:
            this.enterOuterAlt(localctx, 2);
            this.state = 318;
            this.recordType();
            break;

        case 3:
            this.enterOuterAlt(localctx, 3);
            this.state = 319;
            this.tupleType();
            break;

        case 4:
            this.enterOuterAlt(localctx, 4);
            this.state = 320;
            this.functionType();
            break;

        case 5:
            this.enterOuterAlt(localctx, 5);
            this.state = 321;
            this.bindingDeclaration();
            break;

        case 6:
            this.enterOuterAlt(localctx, 6);
            this.state = 322;
            this.subtype();
            break;

        case 7:
            this.enterOuterAlt(localctx, 7);
            this.state = 323;
            this.name();
            break;

        }
    } catch (re) {
    	if(re instanceof antlr4.error.RecognitionException) {
	        localctx.exception = re;
	        this._errHandler.reportError(this, re);
	        this._errHandler.recover(this, re);
	    } else {
	    	throw re;
	    }
    } finally {
        this.exitRule();
    }
    return localctx;
};

function ExprContext(parser, parent, invokingState) {
	if(parent===undefined) {
	    parent = null;
	}
	if(invokingState===undefined || invokingState===null) {
		invokingState = -1;
	}
	antlr4.ParserRuleContext.call(this, parent, invokingState);
    this.parser = parser;
    this.ruleIndex = PvsLanguageParser.RULE_expr;
    return this;
}

ExprContext.prototype = Object.create(antlr4.ParserRuleContext.prototype);
ExprContext.prototype.constructor = ExprContext;

ExprContext.prototype.functionExpression = function() {
    return this.getTypedRuleContext(FunctionExpressionContext,0);
};

ExprContext.prototype.typeExpression = function() {
    return this.getTypedRuleContext(TypeExpressionContext,0);
};

ExprContext.prototype.unaryOp = function() {
    return this.getTypedRuleContext(UnaryOpContext,0);
};

ExprContext.prototype.expr = function(i) {
    if(i===undefined) {
        i = null;
    }
    if(i===null) {
        return this.getTypedRuleContexts(ExprContext);
    } else {
        return this.getTypedRuleContext(ExprContext,i);
    }
};

ExprContext.prototype.TRUE_FALSE = function() {
    return this.getToken(PvsLanguageParser.TRUE_FALSE, 0);
};

ExprContext.prototype.name = function() {
    return this.getTypedRuleContext(NameContext,0);
};

ExprContext.prototype.ifExpression = function() {
    return this.getTypedRuleContext(IfExpressionContext,0);
};

ExprContext.prototype.bindingExpression = function() {
    return this.getTypedRuleContext(BindingExpressionContext,0);
};

ExprContext.prototype.assignmentExpression = function(i) {
    if(i===undefined) {
        i = null;
    }
    if(i===null) {
        return this.getTypedRuleContexts(AssignmentExpressionContext);
    } else {
        return this.getTypedRuleContext(AssignmentExpressionContext,i);
    }
};

ExprContext.prototype.K_LET = function() {
    return this.getToken(PvsLanguageParser.K_LET, 0);
};

ExprContext.prototype.letBindings = function() {
    return this.getTypedRuleContext(LetBindingsContext,0);
};

ExprContext.prototype.K_IN = function() {
    return this.getToken(PvsLanguageParser.K_IN, 0);
};

ExprContext.prototype.NUMBER = function() {
    return this.getToken(PvsLanguageParser.NUMBER, 0);
};

ExprContext.prototype.STRING = function() {
    return this.getToken(PvsLanguageParser.STRING, 0);
};

ExprContext.prototype.binaryOp = function() {
    return this.getTypedRuleContext(BinaryOpContext,0);
};

ExprContext.prototype.arguments = function() {
    return this.getTypedRuleContext(ArgumentsContext,0);
};

ExprContext.prototype.K_WHERE = function() {
    return this.getToken(PvsLanguageParser.K_WHERE, 0);
};

ExprContext.prototype.K_WITH = function() {
    return this.getToken(PvsLanguageParser.K_WITH, 0);
};

ExprContext.prototype.enterRule = function(listener) {
    if(listener instanceof PvsLanguageListener ) {
        listener.enterExpr(this);
	}
};

ExprContext.prototype.exitRule = function(listener) {
    if(listener instanceof PvsLanguageListener ) {
        listener.exitExpr(this);
	}
};



PvsLanguageParser.prototype.expr = function(_p) {
	if(_p===undefined) {
	    _p = 0;
	}
    var _parentctx = this._ctx;
    var _parentState = this.state;
    var localctx = new ExprContext(this, this._ctx, _parentState);
    var _prevctx = localctx;
    var _startState = 44;
    this.enterRecursionRule(localctx, 44, PvsLanguageParser.RULE_expr, _p);
    var _la = 0; // Token type
    try {
        this.enterOuterAlt(localctx, 1);
        this.state = 382;
        var la_ = this._interp.adaptivePredict(this._input,34,this._ctx);
        switch(la_) {
        case 1:
            this.state = 327;
            this.functionExpression();
            break;

        case 2:
            this.state = 328;
            this.typeExpression();
            break;

        case 3:
            this.state = 329;
            this.unaryOp();
            this.state = 331; 
            this._errHandler.sync(this);
            var _alt = 1;
            do {
            	switch (_alt) {
            	case 1:
            		this.state = 330;
            		this.expr(0);
            		break;
            	default:
            		throw new antlr4.error.NoViableAltException(this);
            	}
            	this.state = 333; 
            	this._errHandler.sync(this);
            	_alt = this._interp.adaptivePredict(this._input,29, this._ctx);
            } while ( _alt!=2 && _alt!=antlr4.atn.ATN.INVALID_ALT_NUMBER );
            break;

        case 4:
            this.state = 335;
            this.match(PvsLanguageParser.TRUE_FALSE);
            break;

        case 5:
            this.state = 336;
            this.name();
            break;

        case 6:
            this.state = 337;
            this.ifExpression();
            break;

        case 7:
            this.state = 338;
            this.bindingExpression();
            break;

        case 8:
            this.state = 339;
            this.match(PvsLanguageParser.T__7);
            this.state = 340;
            this.expr(0);
            this.state = 345;
            this._errHandler.sync(this);
            _la = this._input.LA(1);
            while(_la===PvsLanguageParser.T__1) {
                this.state = 341;
                this.match(PvsLanguageParser.T__1);
                this.state = 342;
                this.expr(0);
                this.state = 347;
                this._errHandler.sync(this);
                _la = this._input.LA(1);
            }
            this.state = 348;
            this.match(PvsLanguageParser.T__8);
            break;

        case 9:
            this.state = 350;
            this.match(PvsLanguageParser.T__9);
            this.state = 351;
            this.assignmentExpression();
            this.state = 356;
            this._errHandler.sync(this);
            _la = this._input.LA(1);
            while(_la===PvsLanguageParser.T__1) {
                this.state = 352;
                this.match(PvsLanguageParser.T__1);
                this.state = 353;
                this.assignmentExpression();
                this.state = 358;
                this._errHandler.sync(this);
                _la = this._input.LA(1);
            }
            this.state = 359;
            this.match(PvsLanguageParser.T__10);
            break;

        case 10:
            this.state = 361;
            this.match(PvsLanguageParser.T__4);
            this.state = 362;
            this.expr(0);
            this.state = 367;
            this._errHandler.sync(this);
            _la = this._input.LA(1);
            while(_la===PvsLanguageParser.T__1) {
                this.state = 363;
                this.match(PvsLanguageParser.T__1);
                this.state = 364;
                this.expr(0);
                this.state = 369;
                this._errHandler.sync(this);
                _la = this._input.LA(1);
            }
            this.state = 370;
            this.match(PvsLanguageParser.T__5);
            break;

        case 11:
            this.state = 372;
            this.match(PvsLanguageParser.K_LET);
            this.state = 373;
            this.letBindings();
            this.state = 374;
            this.match(PvsLanguageParser.K_IN);
            this.state = 376; 
            this._errHandler.sync(this);
            var _alt = 1;
            do {
            	switch (_alt) {
            	case 1:
            		this.state = 375;
            		this.expr(0);
            		break;
            	default:
            		throw new antlr4.error.NoViableAltException(this);
            	}
            	this.state = 378; 
            	this._errHandler.sync(this);
            	_alt = this._interp.adaptivePredict(this._input,33, this._ctx);
            } while ( _alt!=2 && _alt!=antlr4.atn.ATN.INVALID_ALT_NUMBER );
            break;

        case 12:
            this.state = 380;
            this.match(PvsLanguageParser.NUMBER);
            break;

        case 13:
            this.state = 381;
            this.match(PvsLanguageParser.STRING);
            break;

        }
        this._ctx.stop = this._input.LT(-1);
        this.state = 411;
        this._errHandler.sync(this);
        var _alt = this._interp.adaptivePredict(this._input,38,this._ctx)
        while(_alt!=2 && _alt!=antlr4.atn.ATN.INVALID_ALT_NUMBER) {
            if(_alt===1) {
                if(this._parseListeners!==null) {
                    this.triggerExitRuleEvent();
                }
                _prevctx = localctx;
                this.state = 409;
                var la_ = this._interp.adaptivePredict(this._input,37,this._ctx);
                switch(la_) {
                case 1:
                    localctx = new ExprContext(this, _parentctx, _parentState);
                    this.pushNewRecursionContext(localctx, _startState, PvsLanguageParser.RULE_expr);
                    this.state = 384;
                    if (!( this.precpred(this._ctx, 17))) {
                        throw new antlr4.error.FailedPredicateException(this, "this.precpred(this._ctx, 17)");
                    }
                    this.state = 385;
                    this.binaryOp();
                    this.state = 387; 
                    this._errHandler.sync(this);
                    var _alt = 1;
                    do {
                    	switch (_alt) {
                    	case 1:
                    		this.state = 386;
                    		this.expr(0);
                    		break;
                    	default:
                    		throw new antlr4.error.NoViableAltException(this);
                    	}
                    	this.state = 389; 
                    	this._errHandler.sync(this);
                    	_alt = this._interp.adaptivePredict(this._input,35, this._ctx);
                    } while ( _alt!=2 && _alt!=antlr4.atn.ATN.INVALID_ALT_NUMBER );
                    break;

                case 2:
                    localctx = new ExprContext(this, _parentctx, _parentState);
                    this.pushNewRecursionContext(localctx, _startState, PvsLanguageParser.RULE_expr);
                    this.state = 391;
                    if (!( this.precpred(this._ctx, 16))) {
                        throw new antlr4.error.FailedPredicateException(this, "this.precpred(this._ctx, 16)");
                    }
                    this.state = 392;
                    this.arguments();
                    break;

                case 3:
                    localctx = new ExprContext(this, _parentctx, _parentState);
                    this.pushNewRecursionContext(localctx, _startState, PvsLanguageParser.RULE_expr);
                    this.state = 393;
                    if (!( this.precpred(this._ctx, 4))) {
                        throw new antlr4.error.FailedPredicateException(this, "this.precpred(this._ctx, 4)");
                    }
                    this.state = 394;
                    this.match(PvsLanguageParser.K_WHERE);
                    this.state = 395;
                    this.letBindings();
                    break;

                case 4:
                    localctx = new ExprContext(this, _parentctx, _parentState);
                    this.pushNewRecursionContext(localctx, _startState, PvsLanguageParser.RULE_expr);
                    this.state = 396;
                    if (!( this.precpred(this._ctx, 3))) {
                        throw new antlr4.error.FailedPredicateException(this, "this.precpred(this._ctx, 3)");
                    }
                    this.state = 397;
                    this.match(PvsLanguageParser.K_WITH);
                    this.state = 398;
                    this.match(PvsLanguageParser.T__0);
                    this.state = 399;
                    this.assignmentExpression();
                    this.state = 404;
                    this._errHandler.sync(this);
                    _la = this._input.LA(1);
                    while(_la===PvsLanguageParser.T__1) {
                        this.state = 400;
                        this.match(PvsLanguageParser.T__1);
                        this.state = 401;
                        this.assignmentExpression();
                        this.state = 406;
                        this._errHandler.sync(this);
                        _la = this._input.LA(1);
                    }
                    this.state = 407;
                    this.match(PvsLanguageParser.T__2);
                    break;

                } 
            }
            this.state = 413;
            this._errHandler.sync(this);
            _alt = this._interp.adaptivePredict(this._input,38,this._ctx);
        }

    } catch( error) {
        if(error instanceof antlr4.error.RecognitionException) {
	        localctx.exception = error;
	        this._errHandler.reportError(this, error);
	        this._errHandler.recover(this, error);
	    } else {
	    	throw error;
	    }
    } finally {
        this.unrollRecursionContexts(_parentctx)
    }
    return localctx;
};

function IfExpressionContext(parser, parent, invokingState) {
	if(parent===undefined) {
	    parent = null;
	}
	if(invokingState===undefined || invokingState===null) {
		invokingState = -1;
	}
	antlr4.ParserRuleContext.call(this, parent, invokingState);
    this.parser = parser;
    this.ruleIndex = PvsLanguageParser.RULE_ifExpression;
    return this;
}

IfExpressionContext.prototype = Object.create(antlr4.ParserRuleContext.prototype);
IfExpressionContext.prototype.constructor = IfExpressionContext;

IfExpressionContext.prototype.K_IF = function() {
    return this.getToken(PvsLanguageParser.K_IF, 0);
};

IfExpressionContext.prototype.expr = function(i) {
    if(i===undefined) {
        i = null;
    }
    if(i===null) {
        return this.getTypedRuleContexts(ExprContext);
    } else {
        return this.getTypedRuleContext(ExprContext,i);
    }
};

IfExpressionContext.prototype.K_THEN = function(i) {
	if(i===undefined) {
		i = null;
	}
    if(i===null) {
        return this.getTokens(PvsLanguageParser.K_THEN);
    } else {
        return this.getToken(PvsLanguageParser.K_THEN, i);
    }
};


IfExpressionContext.prototype.K_ELSE = function() {
    return this.getToken(PvsLanguageParser.K_ELSE, 0);
};

IfExpressionContext.prototype.K_ENDIF = function() {
    return this.getToken(PvsLanguageParser.K_ENDIF, 0);
};

IfExpressionContext.prototype.K_ELSIF = function(i) {
	if(i===undefined) {
		i = null;
	}
    if(i===null) {
        return this.getTokens(PvsLanguageParser.K_ELSIF);
    } else {
        return this.getToken(PvsLanguageParser.K_ELSIF, i);
    }
};


IfExpressionContext.prototype.K_COND = function() {
    return this.getToken(PvsLanguageParser.K_COND, 0);
};

IfExpressionContext.prototype.K_ENDCOND = function() {
    return this.getToken(PvsLanguageParser.K_ENDCOND, 0);
};

IfExpressionContext.prototype.enterRule = function(listener) {
    if(listener instanceof PvsLanguageListener ) {
        listener.enterIfExpression(this);
	}
};

IfExpressionContext.prototype.exitRule = function(listener) {
    if(listener instanceof PvsLanguageListener ) {
        listener.exitIfExpression(this);
	}
};




PvsLanguageParser.IfExpressionContext = IfExpressionContext;

PvsLanguageParser.prototype.ifExpression = function() {

    var localctx = new IfExpressionContext(this, this._ctx, this.state);
    this.enterRule(localctx, 46, PvsLanguageParser.RULE_ifExpression);
    var _la = 0; // Token type
    try {
        this.state = 454;
        switch(this._input.LA(1)) {
        case PvsLanguageParser.K_IF:
            this.enterOuterAlt(localctx, 1);
            this.state = 414;
            this.match(PvsLanguageParser.K_IF);
            this.state = 415;
            this.expr(0);
            this.state = 416;
            this.match(PvsLanguageParser.K_THEN);
            this.state = 417;
            this.expr(0);
            this.state = 425;
            this._errHandler.sync(this);
            _la = this._input.LA(1);
            while(_la===PvsLanguageParser.K_ELSIF) {
                this.state = 418;
                this.match(PvsLanguageParser.K_ELSIF);
                this.state = 419;
                this.expr(0);
                this.state = 420;
                this.match(PvsLanguageParser.K_THEN);
                this.state = 421;
                this.expr(0);
                this.state = 427;
                this._errHandler.sync(this);
                _la = this._input.LA(1);
            }
            this.state = 428;
            this.match(PvsLanguageParser.K_ELSE);
            this.state = 429;
            this.expr(0);
            this.state = 430;
            this.match(PvsLanguageParser.K_ENDIF);
            break;
        case PvsLanguageParser.K_COND:
            this.enterOuterAlt(localctx, 2);
            this.state = 432;
            this.match(PvsLanguageParser.K_COND);
            this.state = 433;
            this.expr(0);
            this.state = 434;
            this.match(PvsLanguageParser.T__11);
            this.state = 435;
            this.expr(0);
            this.state = 443;
            this._errHandler.sync(this);
            var _alt = this._interp.adaptivePredict(this._input,40,this._ctx)
            while(_alt!=2 && _alt!=antlr4.atn.ATN.INVALID_ALT_NUMBER) {
                if(_alt===1) {
                    this.state = 436;
                    this.match(PvsLanguageParser.T__1);
                    this.state = 437;
                    this.expr(0);
                    this.state = 438;
                    this.match(PvsLanguageParser.T__11);
                    this.state = 439;
                    this.expr(0); 
                }
                this.state = 445;
                this._errHandler.sync(this);
                _alt = this._interp.adaptivePredict(this._input,40,this._ctx);
            }

            this.state = 450;
            _la = this._input.LA(1);
            if(_la===PvsLanguageParser.T__1) {
                this.state = 446;
                this.match(PvsLanguageParser.T__1);
                this.state = 447;
                this.match(PvsLanguageParser.K_ELSE);
                this.state = 448;
                this.match(PvsLanguageParser.T__11);
                this.state = 449;
                this.expr(0);
            }

            this.state = 452;
            this.match(PvsLanguageParser.K_ENDCOND);
            break;
        default:
            throw new antlr4.error.NoViableAltException(this);
        }
    } catch (re) {
    	if(re instanceof antlr4.error.RecognitionException) {
	        localctx.exception = re;
	        this._errHandler.reportError(this, re);
	        this._errHandler.recover(this, re);
	    } else {
	    	throw re;
	    }
    } finally {
        this.exitRule();
    }
    return localctx;
};

function MeasureExpressionContext(parser, parent, invokingState) {
	if(parent===undefined) {
	    parent = null;
	}
	if(invokingState===undefined || invokingState===null) {
		invokingState = -1;
	}
	antlr4.ParserRuleContext.call(this, parent, invokingState);
    this.parser = parser;
    this.ruleIndex = PvsLanguageParser.RULE_measureExpression;
    return this;
}

MeasureExpressionContext.prototype = Object.create(antlr4.ParserRuleContext.prototype);
MeasureExpressionContext.prototype.constructor = MeasureExpressionContext;

MeasureExpressionContext.prototype.K_MEASURE = function() {
    return this.getToken(PvsLanguageParser.K_MEASURE, 0);
};

MeasureExpressionContext.prototype.expr = function(i) {
    if(i===undefined) {
        i = null;
    }
    if(i===null) {
        return this.getTypedRuleContexts(ExprContext);
    } else {
        return this.getTypedRuleContext(ExprContext,i);
    }
};

MeasureExpressionContext.prototype.K_BY = function() {
    return this.getToken(PvsLanguageParser.K_BY, 0);
};

MeasureExpressionContext.prototype.enterRule = function(listener) {
    if(listener instanceof PvsLanguageListener ) {
        listener.enterMeasureExpression(this);
	}
};

MeasureExpressionContext.prototype.exitRule = function(listener) {
    if(listener instanceof PvsLanguageListener ) {
        listener.exitMeasureExpression(this);
	}
};




PvsLanguageParser.MeasureExpressionContext = MeasureExpressionContext;

PvsLanguageParser.prototype.measureExpression = function() {

    var localctx = new MeasureExpressionContext(this, this._ctx, this.state);
    this.enterRule(localctx, 48, PvsLanguageParser.RULE_measureExpression);
    var _la = 0; // Token type
    try {
        this.enterOuterAlt(localctx, 1);
        this.state = 456;
        this.match(PvsLanguageParser.K_MEASURE);
        this.state = 457;
        this.expr(0);
        this.state = 460;
        _la = this._input.LA(1);
        if(_la===PvsLanguageParser.K_BY) {
            this.state = 458;
            this.match(PvsLanguageParser.K_BY);
            this.state = 459;
            this.expr(0);
        }

    } catch (re) {
    	if(re instanceof antlr4.error.RecognitionException) {
	        localctx.exception = re;
	        this._errHandler.reportError(this, re);
	        this._errHandler.recover(this, re);
	    } else {
	    	throw re;
	    }
    } finally {
        this.exitRule();
    }
    return localctx;
};

function BindingExpressionContext(parser, parent, invokingState) {
	if(parent===undefined) {
	    parent = null;
	}
	if(invokingState===undefined || invokingState===null) {
		invokingState = -1;
	}
	antlr4.ParserRuleContext.call(this, parent, invokingState);
    this.parser = parser;
    this.ruleIndex = PvsLanguageParser.RULE_bindingExpression;
    return this;
}

BindingExpressionContext.prototype = Object.create(antlr4.ParserRuleContext.prototype);
BindingExpressionContext.prototype.constructor = BindingExpressionContext;

BindingExpressionContext.prototype.lambdaBindings = function() {
    return this.getTypedRuleContext(LambdaBindingsContext,0);
};

BindingExpressionContext.prototype.expr = function() {
    return this.getTypedRuleContext(ExprContext,0);
};

BindingExpressionContext.prototype.K_FORALL = function() {
    return this.getToken(PvsLanguageParser.K_FORALL, 0);
};

BindingExpressionContext.prototype.K_EXISTS = function() {
    return this.getToken(PvsLanguageParser.K_EXISTS, 0);
};

BindingExpressionContext.prototype.K_LAMBDA = function() {
    return this.getToken(PvsLanguageParser.K_LAMBDA, 0);
};

BindingExpressionContext.prototype.enterRule = function(listener) {
    if(listener instanceof PvsLanguageListener ) {
        listener.enterBindingExpression(this);
	}
};

BindingExpressionContext.prototype.exitRule = function(listener) {
    if(listener instanceof PvsLanguageListener ) {
        listener.exitBindingExpression(this);
	}
};




PvsLanguageParser.BindingExpressionContext = BindingExpressionContext;

PvsLanguageParser.prototype.bindingExpression = function() {

    var localctx = new BindingExpressionContext(this, this._ctx, this.state);
    this.enterRule(localctx, 50, PvsLanguageParser.RULE_bindingExpression);
    var _la = 0; // Token type
    try {
        this.enterOuterAlt(localctx, 1);
        this.state = 462;
        _la = this._input.LA(1);
        if(!(((((_la - 44)) & ~0x1f) == 0 && ((1 << (_la - 44)) & ((1 << (PvsLanguageParser.K_LAMBDA - 44)) | (1 << (PvsLanguageParser.K_FORALL - 44)) | (1 << (PvsLanguageParser.K_EXISTS - 44)))) !== 0))) {
        this._errHandler.recoverInline(this);
        }
        else {
            this.consume();
        }
        this.state = 463;
        this.lambdaBindings();
        this.state = 464;
        this.match(PvsLanguageParser.T__3);
        this.state = 465;
        this.expr(0);
    } catch (re) {
    	if(re instanceof antlr4.error.RecognitionException) {
	        localctx.exception = re;
	        this._errHandler.reportError(this, re);
	        this._errHandler.recover(this, re);
	    } else {
	    	throw re;
	    }
    } finally {
        this.exitRule();
    }
    return localctx;
};

function LambdaBindingsContext(parser, parent, invokingState) {
	if(parent===undefined) {
	    parent = null;
	}
	if(invokingState===undefined || invokingState===null) {
		invokingState = -1;
	}
	antlr4.ParserRuleContext.call(this, parent, invokingState);
    this.parser = parser;
    this.ruleIndex = PvsLanguageParser.RULE_lambdaBindings;
    return this;
}

LambdaBindingsContext.prototype = Object.create(antlr4.ParserRuleContext.prototype);
LambdaBindingsContext.prototype.constructor = LambdaBindingsContext;

LambdaBindingsContext.prototype.lambdaBinding = function(i) {
    if(i===undefined) {
        i = null;
    }
    if(i===null) {
        return this.getTypedRuleContexts(LambdaBindingContext);
    } else {
        return this.getTypedRuleContext(LambdaBindingContext,i);
    }
};

LambdaBindingsContext.prototype.enterRule = function(listener) {
    if(listener instanceof PvsLanguageListener ) {
        listener.enterLambdaBindings(this);
	}
};

LambdaBindingsContext.prototype.exitRule = function(listener) {
    if(listener instanceof PvsLanguageListener ) {
        listener.exitLambdaBindings(this);
	}
};




PvsLanguageParser.LambdaBindingsContext = LambdaBindingsContext;

PvsLanguageParser.prototype.lambdaBindings = function() {

    var localctx = new LambdaBindingsContext(this, this._ctx, this.state);
    this.enterRule(localctx, 52, PvsLanguageParser.RULE_lambdaBindings);
    var _la = 0; // Token type
    try {
        this.enterOuterAlt(localctx, 1);
        this.state = 467;
        this.lambdaBinding();
        this.state = 472;
        this._errHandler.sync(this);
        _la = this._input.LA(1);
        while(_la===PvsLanguageParser.T__1) {
            this.state = 468;
            this.match(PvsLanguageParser.T__1);
            this.state = 469;
            this.lambdaBinding();
            this.state = 474;
            this._errHandler.sync(this);
            _la = this._input.LA(1);
        }
    } catch (re) {
    	if(re instanceof antlr4.error.RecognitionException) {
	        localctx.exception = re;
	        this._errHandler.reportError(this, re);
	        this._errHandler.recover(this, re);
	    } else {
	    	throw re;
	    }
    } finally {
        this.exitRule();
    }
    return localctx;
};

function LambdaBindingContext(parser, parent, invokingState) {
	if(parent===undefined) {
	    parent = null;
	}
	if(invokingState===undefined || invokingState===null) {
		invokingState = -1;
	}
	antlr4.ParserRuleContext.call(this, parent, invokingState);
    this.parser = parser;
    this.ruleIndex = PvsLanguageParser.RULE_lambdaBinding;
    return this;
}

LambdaBindingContext.prototype = Object.create(antlr4.ParserRuleContext.prototype);
LambdaBindingContext.prototype.constructor = LambdaBindingContext;

LambdaBindingContext.prototype.identifierOrOperator = function() {
    return this.getTypedRuleContext(IdentifierOrOperatorContext,0);
};

LambdaBindingContext.prototype.bindings = function() {
    return this.getTypedRuleContext(BindingsContext,0);
};

LambdaBindingContext.prototype.enterRule = function(listener) {
    if(listener instanceof PvsLanguageListener ) {
        listener.enterLambdaBinding(this);
	}
};

LambdaBindingContext.prototype.exitRule = function(listener) {
    if(listener instanceof PvsLanguageListener ) {
        listener.exitLambdaBinding(this);
	}
};




PvsLanguageParser.LambdaBindingContext = LambdaBindingContext;

PvsLanguageParser.prototype.lambdaBinding = function() {

    var localctx = new LambdaBindingContext(this, this._ctx, this.state);
    this.enterRule(localctx, 54, PvsLanguageParser.RULE_lambdaBinding);
    try {
        this.state = 477;
        var la_ = this._interp.adaptivePredict(this._input,45,this._ctx);
        switch(la_) {
        case 1:
            this.enterOuterAlt(localctx, 1);
            this.state = 475;
            this.identifierOrOperator();
            break;

        case 2:
            this.enterOuterAlt(localctx, 2);
            this.state = 476;
            this.bindings();
            break;

        }
    } catch (re) {
    	if(re instanceof antlr4.error.RecognitionException) {
	        localctx.exception = re;
	        this._errHandler.reportError(this, re);
	        this._errHandler.recover(this, re);
	    } else {
	    	throw re;
	    }
    } finally {
        this.exitRule();
    }
    return localctx;
};

function BindingsContext(parser, parent, invokingState) {
	if(parent===undefined) {
	    parent = null;
	}
	if(invokingState===undefined || invokingState===null) {
		invokingState = -1;
	}
	antlr4.ParserRuleContext.call(this, parent, invokingState);
    this.parser = parser;
    this.ruleIndex = PvsLanguageParser.RULE_bindings;
    return this;
}

BindingsContext.prototype = Object.create(antlr4.ParserRuleContext.prototype);
BindingsContext.prototype.constructor = BindingsContext;

BindingsContext.prototype.binding = function(i) {
    if(i===undefined) {
        i = null;
    }
    if(i===null) {
        return this.getTypedRuleContexts(BindingContext);
    } else {
        return this.getTypedRuleContext(BindingContext,i);
    }
};

BindingsContext.prototype.enterRule = function(listener) {
    if(listener instanceof PvsLanguageListener ) {
        listener.enterBindings(this);
	}
};

BindingsContext.prototype.exitRule = function(listener) {
    if(listener instanceof PvsLanguageListener ) {
        listener.exitBindings(this);
	}
};




PvsLanguageParser.BindingsContext = BindingsContext;

PvsLanguageParser.prototype.bindings = function() {

    var localctx = new BindingsContext(this, this._ctx, this.state);
    this.enterRule(localctx, 56, PvsLanguageParser.RULE_bindings);
    try {
        this.enterOuterAlt(localctx, 1);
        this.state = 479;
        this.binding();
        this.state = 484;
        this._errHandler.sync(this);
        var _alt = this._interp.adaptivePredict(this._input,46,this._ctx)
        while(_alt!=2 && _alt!=antlr4.atn.ATN.INVALID_ALT_NUMBER) {
            if(_alt===1) {
                this.state = 480;
                this.match(PvsLanguageParser.T__1);
                this.state = 481;
                this.binding(); 
            }
            this.state = 486;
            this._errHandler.sync(this);
            _alt = this._interp.adaptivePredict(this._input,46,this._ctx);
        }

    } catch (re) {
    	if(re instanceof antlr4.error.RecognitionException) {
	        localctx.exception = re;
	        this._errHandler.reportError(this, re);
	        this._errHandler.recover(this, re);
	    } else {
	    	throw re;
	    }
    } finally {
        this.exitRule();
    }
    return localctx;
};

function BindingContext(parser, parent, invokingState) {
	if(parent===undefined) {
	    parent = null;
	}
	if(invokingState===undefined || invokingState===null) {
		invokingState = -1;
	}
	antlr4.ParserRuleContext.call(this, parent, invokingState);
    this.parser = parser;
    this.ruleIndex = PvsLanguageParser.RULE_binding;
    return this;
}

BindingContext.prototype = Object.create(antlr4.ParserRuleContext.prototype);
BindingContext.prototype.constructor = BindingContext;

BindingContext.prototype.typeId = function() {
    return this.getTypedRuleContext(TypeIdContext,0);
};

BindingContext.prototype.typeIds = function() {
    return this.getTypedRuleContext(TypeIdsContext,0);
};

BindingContext.prototype.enterRule = function(listener) {
    if(listener instanceof PvsLanguageListener ) {
        listener.enterBinding(this);
	}
};

BindingContext.prototype.exitRule = function(listener) {
    if(listener instanceof PvsLanguageListener ) {
        listener.exitBinding(this);
	}
};




PvsLanguageParser.BindingContext = BindingContext;

PvsLanguageParser.prototype.binding = function() {

    var localctx = new BindingContext(this, this._ctx, this.state);
    this.enterRule(localctx, 58, PvsLanguageParser.RULE_binding);
    try {
        this.state = 492;
        switch(this._input.LA(1)) {
        case PvsLanguageParser.T__20:
        case PvsLanguageParser.T__21:
        case PvsLanguageParser.T__22:
        case PvsLanguageParser.T__23:
        case PvsLanguageParser.T__24:
        case PvsLanguageParser.T__25:
        case PvsLanguageParser.T__26:
        case PvsLanguageParser.T__27:
        case PvsLanguageParser.T__28:
        case PvsLanguageParser.T__29:
        case PvsLanguageParser.O_IFF:
        case PvsLanguageParser.O_IMPLIES:
        case PvsLanguageParser.O_NOT:
        case PvsLanguageParser.O_AND:
        case PvsLanguageParser.O_OR:
        case PvsLanguageParser.O_NOT_EQUAL:
        case PvsLanguageParser.O_LE:
        case PvsLanguageParser.O_GE:
        case PvsLanguageParser.O_EQUAL:
        case PvsLanguageParser.O_EXP:
        case PvsLanguageParser.O_CONCAT:
        case PvsLanguageParser.ID:
            this.enterOuterAlt(localctx, 1);
            this.state = 487;
            this.typeId();
            break;
        case PvsLanguageParser.T__4:
            this.enterOuterAlt(localctx, 2);
            this.state = 488;
            this.match(PvsLanguageParser.T__4);
            this.state = 489;
            this.typeIds();
            this.state = 490;
            this.match(PvsLanguageParser.T__5);
            break;
        default:
            throw new antlr4.error.NoViableAltException(this);
        }
    } catch (re) {
    	if(re instanceof antlr4.error.RecognitionException) {
	        localctx.exception = re;
	        this._errHandler.reportError(this, re);
	        this._errHandler.recover(this, re);
	    } else {
	    	throw re;
	    }
    } finally {
        this.exitRule();
    }
    return localctx;
};

function TypeIdContext(parser, parent, invokingState) {
	if(parent===undefined) {
	    parent = null;
	}
	if(invokingState===undefined || invokingState===null) {
		invokingState = -1;
	}
	antlr4.ParserRuleContext.call(this, parent, invokingState);
    this.parser = parser;
    this.ruleIndex = PvsLanguageParser.RULE_typeId;
    return this;
}

TypeIdContext.prototype = Object.create(antlr4.ParserRuleContext.prototype);
TypeIdContext.prototype.constructor = TypeIdContext;

TypeIdContext.prototype.identifierOrOperator = function() {
    return this.getTypedRuleContext(IdentifierOrOperatorContext,0);
};

TypeIdContext.prototype.expr = function(i) {
    if(i===undefined) {
        i = null;
    }
    if(i===null) {
        return this.getTypedRuleContexts(ExprContext);
    } else {
        return this.getTypedRuleContext(ExprContext,i);
    }
};

TypeIdContext.prototype.enterRule = function(listener) {
    if(listener instanceof PvsLanguageListener ) {
        listener.enterTypeId(this);
	}
};

TypeIdContext.prototype.exitRule = function(listener) {
    if(listener instanceof PvsLanguageListener ) {
        listener.exitTypeId(this);
	}
};




PvsLanguageParser.TypeIdContext = TypeIdContext;

PvsLanguageParser.prototype.typeId = function() {

    var localctx = new TypeIdContext(this, this._ctx, this.state);
    this.enterRule(localctx, 60, PvsLanguageParser.RULE_typeId);
    var _la = 0; // Token type
    try {
        this.enterOuterAlt(localctx, 1);
        this.state = 494;
        this.identifierOrOperator();
        this.state = 501;
        var la_ = this._interp.adaptivePredict(this._input,49,this._ctx);
        if(la_===1) {
            this.state = 495;
            this.match(PvsLanguageParser.T__3);
            this.state = 497; 
            this._errHandler.sync(this);
            var _alt = 1;
            do {
            	switch (_alt) {
            	case 1:
            		this.state = 496;
            		this.expr(0);
            		break;
            	default:
            		throw new antlr4.error.NoViableAltException(this);
            	}
            	this.state = 499; 
            	this._errHandler.sync(this);
            	_alt = this._interp.adaptivePredict(this._input,48, this._ctx);
            } while ( _alt!=2 && _alt!=antlr4.atn.ATN.INVALID_ALT_NUMBER );

        }
        this.state = 509;
        _la = this._input.LA(1);
        if(_la===PvsLanguageParser.T__12) {
            this.state = 503;
            this.match(PvsLanguageParser.T__12);
            this.state = 505; 
            this._errHandler.sync(this);
            var _alt = 1;
            do {
            	switch (_alt) {
            	case 1:
            		this.state = 504;
            		this.expr(0);
            		break;
            	default:
            		throw new antlr4.error.NoViableAltException(this);
            	}
            	this.state = 507; 
            	this._errHandler.sync(this);
            	_alt = this._interp.adaptivePredict(this._input,50, this._ctx);
            } while ( _alt!=2 && _alt!=antlr4.atn.ATN.INVALID_ALT_NUMBER );
        }

    } catch (re) {
    	if(re instanceof antlr4.error.RecognitionException) {
	        localctx.exception = re;
	        this._errHandler.reportError(this, re);
	        this._errHandler.recover(this, re);
	    } else {
	    	throw re;
	    }
    } finally {
        this.exitRule();
    }
    return localctx;
};

function TypeIdsContext(parser, parent, invokingState) {
	if(parent===undefined) {
	    parent = null;
	}
	if(invokingState===undefined || invokingState===null) {
		invokingState = -1;
	}
	antlr4.ParserRuleContext.call(this, parent, invokingState);
    this.parser = parser;
    this.ruleIndex = PvsLanguageParser.RULE_typeIds;
    return this;
}

TypeIdsContext.prototype = Object.create(antlr4.ParserRuleContext.prototype);
TypeIdsContext.prototype.constructor = TypeIdsContext;

TypeIdsContext.prototype.identifierOrOperators = function() {
    return this.getTypedRuleContext(IdentifierOrOperatorsContext,0);
};

TypeIdsContext.prototype.expr = function(i) {
    if(i===undefined) {
        i = null;
    }
    if(i===null) {
        return this.getTypedRuleContexts(ExprContext);
    } else {
        return this.getTypedRuleContext(ExprContext,i);
    }
};

TypeIdsContext.prototype.enterRule = function(listener) {
    if(listener instanceof PvsLanguageListener ) {
        listener.enterTypeIds(this);
	}
};

TypeIdsContext.prototype.exitRule = function(listener) {
    if(listener instanceof PvsLanguageListener ) {
        listener.exitTypeIds(this);
	}
};




PvsLanguageParser.TypeIdsContext = TypeIdsContext;

PvsLanguageParser.prototype.typeIds = function() {

    var localctx = new TypeIdsContext(this, this._ctx, this.state);
    this.enterRule(localctx, 62, PvsLanguageParser.RULE_typeIds);
    var _la = 0; // Token type
    try {
        this.enterOuterAlt(localctx, 1);
        this.state = 511;
        this.identifierOrOperators();
        this.state = 518;
        _la = this._input.LA(1);
        if(_la===PvsLanguageParser.T__3) {
            this.state = 512;
            this.match(PvsLanguageParser.T__3);
            this.state = 514; 
            this._errHandler.sync(this);
            _la = this._input.LA(1);
            do {
                this.state = 513;
                this.expr(0);
                this.state = 516; 
                this._errHandler.sync(this);
                _la = this._input.LA(1);
            } while((((_la) & ~0x1f) == 0 && ((1 << _la) & ((1 << PvsLanguageParser.T__0) | (1 << PvsLanguageParser.T__4) | (1 << PvsLanguageParser.T__7) | (1 << PvsLanguageParser.T__9) | (1 << PvsLanguageParser.T__15) | (1 << PvsLanguageParser.T__17) | (1 << PvsLanguageParser.T__20) | (1 << PvsLanguageParser.T__21) | (1 << PvsLanguageParser.T__22) | (1 << PvsLanguageParser.T__23) | (1 << PvsLanguageParser.T__24) | (1 << PvsLanguageParser.T__25) | (1 << PvsLanguageParser.T__26) | (1 << PvsLanguageParser.T__27) | (1 << PvsLanguageParser.T__28) | (1 << PvsLanguageParser.T__29))) !== 0) || ((((_la - 39)) & ~0x1f) == 0 && ((1 << (_la - 39)) & ((1 << (PvsLanguageParser.K_IF - 39)) | (1 << (PvsLanguageParser.K_LAMBDA - 39)) | (1 << (PvsLanguageParser.K_FORALL - 39)) | (1 << (PvsLanguageParser.K_EXISTS - 39)) | (1 << (PvsLanguageParser.K_FUNCTION - 39)) | (1 << (PvsLanguageParser.K_ARRAY - 39)) | (1 << (PvsLanguageParser.K_LET - 39)) | (1 << (PvsLanguageParser.K_COND - 39)) | (1 << (PvsLanguageParser.O_IFF - 39)))) !== 0) || ((((_la - 71)) & ~0x1f) == 0 && ((1 << (_la - 71)) & ((1 << (PvsLanguageParser.O_IMPLIES - 71)) | (1 << (PvsLanguageParser.O_NOT - 71)) | (1 << (PvsLanguageParser.O_AND - 71)) | (1 << (PvsLanguageParser.O_OR - 71)) | (1 << (PvsLanguageParser.O_NOT_EQUAL - 71)) | (1 << (PvsLanguageParser.O_LE - 71)) | (1 << (PvsLanguageParser.O_GE - 71)) | (1 << (PvsLanguageParser.O_EQUAL - 71)) | (1 << (PvsLanguageParser.O_EXP - 71)) | (1 << (PvsLanguageParser.O_CONCAT - 71)) | (1 << (PvsLanguageParser.TRUE_FALSE - 71)) | (1 << (PvsLanguageParser.STRING - 71)) | (1 << (PvsLanguageParser.NUMBER - 71)) | (1 << (PvsLanguageParser.ID - 71)))) !== 0));
        }

        this.state = 526;
        _la = this._input.LA(1);
        if(_la===PvsLanguageParser.T__12) {
            this.state = 520;
            this.match(PvsLanguageParser.T__12);
            this.state = 522; 
            this._errHandler.sync(this);
            _la = this._input.LA(1);
            do {
                this.state = 521;
                this.expr(0);
                this.state = 524; 
                this._errHandler.sync(this);
                _la = this._input.LA(1);
            } while((((_la) & ~0x1f) == 0 && ((1 << _la) & ((1 << PvsLanguageParser.T__0) | (1 << PvsLanguageParser.T__4) | (1 << PvsLanguageParser.T__7) | (1 << PvsLanguageParser.T__9) | (1 << PvsLanguageParser.T__15) | (1 << PvsLanguageParser.T__17) | (1 << PvsLanguageParser.T__20) | (1 << PvsLanguageParser.T__21) | (1 << PvsLanguageParser.T__22) | (1 << PvsLanguageParser.T__23) | (1 << PvsLanguageParser.T__24) | (1 << PvsLanguageParser.T__25) | (1 << PvsLanguageParser.T__26) | (1 << PvsLanguageParser.T__27) | (1 << PvsLanguageParser.T__28) | (1 << PvsLanguageParser.T__29))) !== 0) || ((((_la - 39)) & ~0x1f) == 0 && ((1 << (_la - 39)) & ((1 << (PvsLanguageParser.K_IF - 39)) | (1 << (PvsLanguageParser.K_LAMBDA - 39)) | (1 << (PvsLanguageParser.K_FORALL - 39)) | (1 << (PvsLanguageParser.K_EXISTS - 39)) | (1 << (PvsLanguageParser.K_FUNCTION - 39)) | (1 << (PvsLanguageParser.K_ARRAY - 39)) | (1 << (PvsLanguageParser.K_LET - 39)) | (1 << (PvsLanguageParser.K_COND - 39)) | (1 << (PvsLanguageParser.O_IFF - 39)))) !== 0) || ((((_la - 71)) & ~0x1f) == 0 && ((1 << (_la - 71)) & ((1 << (PvsLanguageParser.O_IMPLIES - 71)) | (1 << (PvsLanguageParser.O_NOT - 71)) | (1 << (PvsLanguageParser.O_AND - 71)) | (1 << (PvsLanguageParser.O_OR - 71)) | (1 << (PvsLanguageParser.O_NOT_EQUAL - 71)) | (1 << (PvsLanguageParser.O_LE - 71)) | (1 << (PvsLanguageParser.O_GE - 71)) | (1 << (PvsLanguageParser.O_EQUAL - 71)) | (1 << (PvsLanguageParser.O_EXP - 71)) | (1 << (PvsLanguageParser.O_CONCAT - 71)) | (1 << (PvsLanguageParser.TRUE_FALSE - 71)) | (1 << (PvsLanguageParser.STRING - 71)) | (1 << (PvsLanguageParser.NUMBER - 71)) | (1 << (PvsLanguageParser.ID - 71)))) !== 0));
        }

    } catch (re) {
    	if(re instanceof antlr4.error.RecognitionException) {
	        localctx.exception = re;
	        this._errHandler.reportError(this, re);
	        this._errHandler.recover(this, re);
	    } else {
	    	throw re;
	    }
    } finally {
        this.exitRule();
    }
    return localctx;
};

function LetBindingsContext(parser, parent, invokingState) {
	if(parent===undefined) {
	    parent = null;
	}
	if(invokingState===undefined || invokingState===null) {
		invokingState = -1;
	}
	antlr4.ParserRuleContext.call(this, parent, invokingState);
    this.parser = parser;
    this.ruleIndex = PvsLanguageParser.RULE_letBindings;
    return this;
}

LetBindingsContext.prototype = Object.create(antlr4.ParserRuleContext.prototype);
LetBindingsContext.prototype.constructor = LetBindingsContext;

LetBindingsContext.prototype.letBinding = function(i) {
    if(i===undefined) {
        i = null;
    }
    if(i===null) {
        return this.getTypedRuleContexts(LetBindingContext);
    } else {
        return this.getTypedRuleContext(LetBindingContext,i);
    }
};

LetBindingsContext.prototype.enterRule = function(listener) {
    if(listener instanceof PvsLanguageListener ) {
        listener.enterLetBindings(this);
	}
};

LetBindingsContext.prototype.exitRule = function(listener) {
    if(listener instanceof PvsLanguageListener ) {
        listener.exitLetBindings(this);
	}
};




PvsLanguageParser.LetBindingsContext = LetBindingsContext;

PvsLanguageParser.prototype.letBindings = function() {

    var localctx = new LetBindingsContext(this, this._ctx, this.state);
    this.enterRule(localctx, 64, PvsLanguageParser.RULE_letBindings);
    try {
        this.enterOuterAlt(localctx, 1);
        this.state = 528;
        this.letBinding();
        this.state = 533;
        this._errHandler.sync(this);
        var _alt = this._interp.adaptivePredict(this._input,56,this._ctx)
        while(_alt!=2 && _alt!=antlr4.atn.ATN.INVALID_ALT_NUMBER) {
            if(_alt===1) {
                this.state = 529;
                this.match(PvsLanguageParser.T__1);
                this.state = 530;
                this.letBinding(); 
            }
            this.state = 535;
            this._errHandler.sync(this);
            _alt = this._interp.adaptivePredict(this._input,56,this._ctx);
        }

    } catch (re) {
    	if(re instanceof antlr4.error.RecognitionException) {
	        localctx.exception = re;
	        this._errHandler.reportError(this, re);
	        this._errHandler.recover(this, re);
	    } else {
	    	throw re;
	    }
    } finally {
        this.exitRule();
    }
    return localctx;
};

function LetBindingContext(parser, parent, invokingState) {
	if(parent===undefined) {
	    parent = null;
	}
	if(invokingState===undefined || invokingState===null) {
		invokingState = -1;
	}
	antlr4.ParserRuleContext.call(this, parent, invokingState);
    this.parser = parser;
    this.ruleIndex = PvsLanguageParser.RULE_letBinding;
    return this;
}

LetBindingContext.prototype = Object.create(antlr4.ParserRuleContext.prototype);
LetBindingContext.prototype.constructor = LetBindingContext;

LetBindingContext.prototype.letBind = function(i) {
    if(i===undefined) {
        i = null;
    }
    if(i===null) {
        return this.getTypedRuleContexts(LetBindContext);
    } else {
        return this.getTypedRuleContext(LetBindContext,i);
    }
};

LetBindingContext.prototype.expr = function() {
    return this.getTypedRuleContext(ExprContext,0);
};

LetBindingContext.prototype.enterRule = function(listener) {
    if(listener instanceof PvsLanguageListener ) {
        listener.enterLetBinding(this);
	}
};

LetBindingContext.prototype.exitRule = function(listener) {
    if(listener instanceof PvsLanguageListener ) {
        listener.exitLetBinding(this);
	}
};




PvsLanguageParser.LetBindingContext = LetBindingContext;

PvsLanguageParser.prototype.letBinding = function() {

    var localctx = new LetBindingContext(this, this._ctx, this.state);
    this.enterRule(localctx, 66, PvsLanguageParser.RULE_letBinding);
    var _la = 0; // Token type
    try {
        this.enterOuterAlt(localctx, 1);
        this.state = 536;
        this.letBind();
        this.state = 541;
        this._errHandler.sync(this);
        _la = this._input.LA(1);
        while(_la===PvsLanguageParser.T__1) {
            this.state = 537;
            this.match(PvsLanguageParser.T__1);
            this.state = 538;
            this.letBind();
            this.state = 543;
            this._errHandler.sync(this);
            _la = this._input.LA(1);
        }
        this.state = 544;
        this.match(PvsLanguageParser.O_EQUAL);
        this.state = 545;
        this.expr(0);
    } catch (re) {
    	if(re instanceof antlr4.error.RecognitionException) {
	        localctx.exception = re;
	        this._errHandler.reportError(this, re);
	        this._errHandler.recover(this, re);
	    } else {
	    	throw re;
	    }
    } finally {
        this.exitRule();
    }
    return localctx;
};

function LetBindContext(parser, parent, invokingState) {
	if(parent===undefined) {
	    parent = null;
	}
	if(invokingState===undefined || invokingState===null) {
		invokingState = -1;
	}
	antlr4.ParserRuleContext.call(this, parent, invokingState);
    this.parser = parser;
    this.ruleIndex = PvsLanguageParser.RULE_letBind;
    return this;
}

LetBindContext.prototype = Object.create(antlr4.ParserRuleContext.prototype);
LetBindContext.prototype.constructor = LetBindContext;

LetBindContext.prototype.identifierOrOperator = function() {
    return this.getTypedRuleContext(IdentifierOrOperatorContext,0);
};

LetBindContext.prototype.bindings = function(i) {
    if(i===undefined) {
        i = null;
    }
    if(i===null) {
        return this.getTypedRuleContexts(BindingsContext);
    } else {
        return this.getTypedRuleContext(BindingsContext,i);
    }
};

LetBindContext.prototype.expr = function() {
    return this.getTypedRuleContext(ExprContext,0);
};

LetBindContext.prototype.enterRule = function(listener) {
    if(listener instanceof PvsLanguageListener ) {
        listener.enterLetBind(this);
	}
};

LetBindContext.prototype.exitRule = function(listener) {
    if(listener instanceof PvsLanguageListener ) {
        listener.exitLetBind(this);
	}
};




PvsLanguageParser.LetBindContext = LetBindContext;

PvsLanguageParser.prototype.letBind = function() {

    var localctx = new LetBindContext(this, this._ctx, this.state);
    this.enterRule(localctx, 68, PvsLanguageParser.RULE_letBind);
    var _la = 0; // Token type
    try {
        this.enterOuterAlt(localctx, 1);
        this.state = 548;
        var la_ = this._interp.adaptivePredict(this._input,58,this._ctx);
        if(la_===1) {
            this.state = 547;
            this.identifierOrOperator();

        }
        this.state = 553;
        this._errHandler.sync(this);
        var _alt = this._interp.adaptivePredict(this._input,59,this._ctx)
        while(_alt!=2 && _alt!=antlr4.atn.ATN.INVALID_ALT_NUMBER) {
            if(_alt===1) {
                this.state = 550;
                this.bindings(); 
            }
            this.state = 555;
            this._errHandler.sync(this);
            _alt = this._interp.adaptivePredict(this._input,59,this._ctx);
        }

        this.state = 558;
        _la = this._input.LA(1);
        if(_la===PvsLanguageParser.T__3) {
            this.state = 556;
            this.match(PvsLanguageParser.T__3);
            this.state = 557;
            this.expr(0);
        }

    } catch (re) {
    	if(re instanceof antlr4.error.RecognitionException) {
	        localctx.exception = re;
	        this._errHandler.reportError(this, re);
	        this._errHandler.recover(this, re);
	    } else {
	    	throw re;
	    }
    } finally {
        this.exitRule();
    }
    return localctx;
};

function FunctionExpressionContext(parser, parent, invokingState) {
	if(parent===undefined) {
	    parent = null;
	}
	if(invokingState===undefined || invokingState===null) {
		invokingState = -1;
	}
	antlr4.ParserRuleContext.call(this, parent, invokingState);
    this.parser = parser;
    this.ruleIndex = PvsLanguageParser.RULE_functionExpression;
    return this;
}

FunctionExpressionContext.prototype = Object.create(antlr4.ParserRuleContext.prototype);
FunctionExpressionContext.prototype.constructor = FunctionExpressionContext;

FunctionExpressionContext.prototype.identifier = function() {
    return this.getTypedRuleContext(IdentifierContext,0);
};

FunctionExpressionContext.prototype.actuals = function() {
    return this.getTypedRuleContext(ActualsContext,0);
};

FunctionExpressionContext.prototype.expr = function(i) {
    if(i===undefined) {
        i = null;
    }
    if(i===null) {
        return this.getTypedRuleContexts(ExprContext);
    } else {
        return this.getTypedRuleContext(ExprContext,i);
    }
};

FunctionExpressionContext.prototype.enterRule = function(listener) {
    if(listener instanceof PvsLanguageListener ) {
        listener.enterFunctionExpression(this);
	}
};

FunctionExpressionContext.prototype.exitRule = function(listener) {
    if(listener instanceof PvsLanguageListener ) {
        listener.exitFunctionExpression(this);
	}
};




PvsLanguageParser.FunctionExpressionContext = FunctionExpressionContext;

PvsLanguageParser.prototype.functionExpression = function() {

    var localctx = new FunctionExpressionContext(this, this._ctx, this.state);
    this.enterRule(localctx, 70, PvsLanguageParser.RULE_functionExpression);
    var _la = 0; // Token type
    try {
        this.enterOuterAlt(localctx, 1);
        this.state = 560;
        this.identifier();
        this.state = 562;
        _la = this._input.LA(1);
        if(_la===PvsLanguageParser.T__0) {
            this.state = 561;
            this.actuals();
        }

        this.state = 564;
        this.match(PvsLanguageParser.T__4);
        this.state = 566; 
        this._errHandler.sync(this);
        _la = this._input.LA(1);
        do {
            this.state = 565;
            this.expr(0);
            this.state = 568; 
            this._errHandler.sync(this);
            _la = this._input.LA(1);
        } while((((_la) & ~0x1f) == 0 && ((1 << _la) & ((1 << PvsLanguageParser.T__0) | (1 << PvsLanguageParser.T__4) | (1 << PvsLanguageParser.T__7) | (1 << PvsLanguageParser.T__9) | (1 << PvsLanguageParser.T__15) | (1 << PvsLanguageParser.T__17) | (1 << PvsLanguageParser.T__20) | (1 << PvsLanguageParser.T__21) | (1 << PvsLanguageParser.T__22) | (1 << PvsLanguageParser.T__23) | (1 << PvsLanguageParser.T__24) | (1 << PvsLanguageParser.T__25) | (1 << PvsLanguageParser.T__26) | (1 << PvsLanguageParser.T__27) | (1 << PvsLanguageParser.T__28) | (1 << PvsLanguageParser.T__29))) !== 0) || ((((_la - 39)) & ~0x1f) == 0 && ((1 << (_la - 39)) & ((1 << (PvsLanguageParser.K_IF - 39)) | (1 << (PvsLanguageParser.K_LAMBDA - 39)) | (1 << (PvsLanguageParser.K_FORALL - 39)) | (1 << (PvsLanguageParser.K_EXISTS - 39)) | (1 << (PvsLanguageParser.K_FUNCTION - 39)) | (1 << (PvsLanguageParser.K_ARRAY - 39)) | (1 << (PvsLanguageParser.K_LET - 39)) | (1 << (PvsLanguageParser.K_COND - 39)) | (1 << (PvsLanguageParser.O_IFF - 39)))) !== 0) || ((((_la - 71)) & ~0x1f) == 0 && ((1 << (_la - 71)) & ((1 << (PvsLanguageParser.O_IMPLIES - 71)) | (1 << (PvsLanguageParser.O_NOT - 71)) | (1 << (PvsLanguageParser.O_AND - 71)) | (1 << (PvsLanguageParser.O_OR - 71)) | (1 << (PvsLanguageParser.O_NOT_EQUAL - 71)) | (1 << (PvsLanguageParser.O_LE - 71)) | (1 << (PvsLanguageParser.O_GE - 71)) | (1 << (PvsLanguageParser.O_EQUAL - 71)) | (1 << (PvsLanguageParser.O_EXP - 71)) | (1 << (PvsLanguageParser.O_CONCAT - 71)) | (1 << (PvsLanguageParser.TRUE_FALSE - 71)) | (1 << (PvsLanguageParser.STRING - 71)) | (1 << (PvsLanguageParser.NUMBER - 71)) | (1 << (PvsLanguageParser.ID - 71)))) !== 0));
        this.state = 578;
        this._errHandler.sync(this);
        _la = this._input.LA(1);
        while(_la===PvsLanguageParser.T__1) {
            this.state = 570;
            this.match(PvsLanguageParser.T__1);
            this.state = 572; 
            this._errHandler.sync(this);
            _la = this._input.LA(1);
            do {
                this.state = 571;
                this.expr(0);
                this.state = 574; 
                this._errHandler.sync(this);
                _la = this._input.LA(1);
            } while((((_la) & ~0x1f) == 0 && ((1 << _la) & ((1 << PvsLanguageParser.T__0) | (1 << PvsLanguageParser.T__4) | (1 << PvsLanguageParser.T__7) | (1 << PvsLanguageParser.T__9) | (1 << PvsLanguageParser.T__15) | (1 << PvsLanguageParser.T__17) | (1 << PvsLanguageParser.T__20) | (1 << PvsLanguageParser.T__21) | (1 << PvsLanguageParser.T__22) | (1 << PvsLanguageParser.T__23) | (1 << PvsLanguageParser.T__24) | (1 << PvsLanguageParser.T__25) | (1 << PvsLanguageParser.T__26) | (1 << PvsLanguageParser.T__27) | (1 << PvsLanguageParser.T__28) | (1 << PvsLanguageParser.T__29))) !== 0) || ((((_la - 39)) & ~0x1f) == 0 && ((1 << (_la - 39)) & ((1 << (PvsLanguageParser.K_IF - 39)) | (1 << (PvsLanguageParser.K_LAMBDA - 39)) | (1 << (PvsLanguageParser.K_FORALL - 39)) | (1 << (PvsLanguageParser.K_EXISTS - 39)) | (1 << (PvsLanguageParser.K_FUNCTION - 39)) | (1 << (PvsLanguageParser.K_ARRAY - 39)) | (1 << (PvsLanguageParser.K_LET - 39)) | (1 << (PvsLanguageParser.K_COND - 39)) | (1 << (PvsLanguageParser.O_IFF - 39)))) !== 0) || ((((_la - 71)) & ~0x1f) == 0 && ((1 << (_la - 71)) & ((1 << (PvsLanguageParser.O_IMPLIES - 71)) | (1 << (PvsLanguageParser.O_NOT - 71)) | (1 << (PvsLanguageParser.O_AND - 71)) | (1 << (PvsLanguageParser.O_OR - 71)) | (1 << (PvsLanguageParser.O_NOT_EQUAL - 71)) | (1 << (PvsLanguageParser.O_LE - 71)) | (1 << (PvsLanguageParser.O_GE - 71)) | (1 << (PvsLanguageParser.O_EQUAL - 71)) | (1 << (PvsLanguageParser.O_EXP - 71)) | (1 << (PvsLanguageParser.O_CONCAT - 71)) | (1 << (PvsLanguageParser.TRUE_FALSE - 71)) | (1 << (PvsLanguageParser.STRING - 71)) | (1 << (PvsLanguageParser.NUMBER - 71)) | (1 << (PvsLanguageParser.ID - 71)))) !== 0));
            this.state = 580;
            this._errHandler.sync(this);
            _la = this._input.LA(1);
        }
        this.state = 581;
        this.match(PvsLanguageParser.T__5);
        this.state = 603;
        this._errHandler.sync(this);
        var _alt = this._interp.adaptivePredict(this._input,68,this._ctx)
        while(_alt!=2 && _alt!=antlr4.atn.ATN.INVALID_ALT_NUMBER) {
            if(_alt===1) {
                this.state = 582;
                this.match(PvsLanguageParser.T__4);
                this.state = 584; 
                this._errHandler.sync(this);
                _la = this._input.LA(1);
                do {
                    this.state = 583;
                    this.expr(0);
                    this.state = 586; 
                    this._errHandler.sync(this);
                    _la = this._input.LA(1);
                } while((((_la) & ~0x1f) == 0 && ((1 << _la) & ((1 << PvsLanguageParser.T__0) | (1 << PvsLanguageParser.T__4) | (1 << PvsLanguageParser.T__7) | (1 << PvsLanguageParser.T__9) | (1 << PvsLanguageParser.T__15) | (1 << PvsLanguageParser.T__17) | (1 << PvsLanguageParser.T__20) | (1 << PvsLanguageParser.T__21) | (1 << PvsLanguageParser.T__22) | (1 << PvsLanguageParser.T__23) | (1 << PvsLanguageParser.T__24) | (1 << PvsLanguageParser.T__25) | (1 << PvsLanguageParser.T__26) | (1 << PvsLanguageParser.T__27) | (1 << PvsLanguageParser.T__28) | (1 << PvsLanguageParser.T__29))) !== 0) || ((((_la - 39)) & ~0x1f) == 0 && ((1 << (_la - 39)) & ((1 << (PvsLanguageParser.K_IF - 39)) | (1 << (PvsLanguageParser.K_LAMBDA - 39)) | (1 << (PvsLanguageParser.K_FORALL - 39)) | (1 << (PvsLanguageParser.K_EXISTS - 39)) | (1 << (PvsLanguageParser.K_FUNCTION - 39)) | (1 << (PvsLanguageParser.K_ARRAY - 39)) | (1 << (PvsLanguageParser.K_LET - 39)) | (1 << (PvsLanguageParser.K_COND - 39)) | (1 << (PvsLanguageParser.O_IFF - 39)))) !== 0) || ((((_la - 71)) & ~0x1f) == 0 && ((1 << (_la - 71)) & ((1 << (PvsLanguageParser.O_IMPLIES - 71)) | (1 << (PvsLanguageParser.O_NOT - 71)) | (1 << (PvsLanguageParser.O_AND - 71)) | (1 << (PvsLanguageParser.O_OR - 71)) | (1 << (PvsLanguageParser.O_NOT_EQUAL - 71)) | (1 << (PvsLanguageParser.O_LE - 71)) | (1 << (PvsLanguageParser.O_GE - 71)) | (1 << (PvsLanguageParser.O_EQUAL - 71)) | (1 << (PvsLanguageParser.O_EXP - 71)) | (1 << (PvsLanguageParser.O_CONCAT - 71)) | (1 << (PvsLanguageParser.TRUE_FALSE - 71)) | (1 << (PvsLanguageParser.STRING - 71)) | (1 << (PvsLanguageParser.NUMBER - 71)) | (1 << (PvsLanguageParser.ID - 71)))) !== 0));
                this.state = 596;
                this._errHandler.sync(this);
                _la = this._input.LA(1);
                while(_la===PvsLanguageParser.T__1) {
                    this.state = 588;
                    this.match(PvsLanguageParser.T__1);
                    this.state = 590; 
                    this._errHandler.sync(this);
                    _la = this._input.LA(1);
                    do {
                        this.state = 589;
                        this.expr(0);
                        this.state = 592; 
                        this._errHandler.sync(this);
                        _la = this._input.LA(1);
                    } while((((_la) & ~0x1f) == 0 && ((1 << _la) & ((1 << PvsLanguageParser.T__0) | (1 << PvsLanguageParser.T__4) | (1 << PvsLanguageParser.T__7) | (1 << PvsLanguageParser.T__9) | (1 << PvsLanguageParser.T__15) | (1 << PvsLanguageParser.T__17) | (1 << PvsLanguageParser.T__20) | (1 << PvsLanguageParser.T__21) | (1 << PvsLanguageParser.T__22) | (1 << PvsLanguageParser.T__23) | (1 << PvsLanguageParser.T__24) | (1 << PvsLanguageParser.T__25) | (1 << PvsLanguageParser.T__26) | (1 << PvsLanguageParser.T__27) | (1 << PvsLanguageParser.T__28) | (1 << PvsLanguageParser.T__29))) !== 0) || ((((_la - 39)) & ~0x1f) == 0 && ((1 << (_la - 39)) & ((1 << (PvsLanguageParser.K_IF - 39)) | (1 << (PvsLanguageParser.K_LAMBDA - 39)) | (1 << (PvsLanguageParser.K_FORALL - 39)) | (1 << (PvsLanguageParser.K_EXISTS - 39)) | (1 << (PvsLanguageParser.K_FUNCTION - 39)) | (1 << (PvsLanguageParser.K_ARRAY - 39)) | (1 << (PvsLanguageParser.K_LET - 39)) | (1 << (PvsLanguageParser.K_COND - 39)) | (1 << (PvsLanguageParser.O_IFF - 39)))) !== 0) || ((((_la - 71)) & ~0x1f) == 0 && ((1 << (_la - 71)) & ((1 << (PvsLanguageParser.O_IMPLIES - 71)) | (1 << (PvsLanguageParser.O_NOT - 71)) | (1 << (PvsLanguageParser.O_AND - 71)) | (1 << (PvsLanguageParser.O_OR - 71)) | (1 << (PvsLanguageParser.O_NOT_EQUAL - 71)) | (1 << (PvsLanguageParser.O_LE - 71)) | (1 << (PvsLanguageParser.O_GE - 71)) | (1 << (PvsLanguageParser.O_EQUAL - 71)) | (1 << (PvsLanguageParser.O_EXP - 71)) | (1 << (PvsLanguageParser.O_CONCAT - 71)) | (1 << (PvsLanguageParser.TRUE_FALSE - 71)) | (1 << (PvsLanguageParser.STRING - 71)) | (1 << (PvsLanguageParser.NUMBER - 71)) | (1 << (PvsLanguageParser.ID - 71)))) !== 0));
                    this.state = 598;
                    this._errHandler.sync(this);
                    _la = this._input.LA(1);
                }
                this.state = 599;
                this.match(PvsLanguageParser.T__5); 
            }
            this.state = 605;
            this._errHandler.sync(this);
            _alt = this._interp.adaptivePredict(this._input,68,this._ctx);
        }

    } catch (re) {
    	if(re instanceof antlr4.error.RecognitionException) {
	        localctx.exception = re;
	        this._errHandler.reportError(this, re);
	        this._errHandler.recover(this, re);
	    } else {
	    	throw re;
	    }
    } finally {
        this.exitRule();
    }
    return localctx;
};

function AssignmentExpressionContext(parser, parent, invokingState) {
	if(parent===undefined) {
	    parent = null;
	}
	if(invokingState===undefined || invokingState===null) {
		invokingState = -1;
	}
	antlr4.ParserRuleContext.call(this, parent, invokingState);
    this.parser = parser;
    this.ruleIndex = PvsLanguageParser.RULE_assignmentExpression;
    return this;
}

AssignmentExpressionContext.prototype = Object.create(antlr4.ParserRuleContext.prototype);
AssignmentExpressionContext.prototype.constructor = AssignmentExpressionContext;

AssignmentExpressionContext.prototype.identifier = function() {
    return this.getTypedRuleContext(IdentifierContext,0);
};

AssignmentExpressionContext.prototype.expr = function() {
    return this.getTypedRuleContext(ExprContext,0);
};

AssignmentExpressionContext.prototype.enterRule = function(listener) {
    if(listener instanceof PvsLanguageListener ) {
        listener.enterAssignmentExpression(this);
	}
};

AssignmentExpressionContext.prototype.exitRule = function(listener) {
    if(listener instanceof PvsLanguageListener ) {
        listener.exitAssignmentExpression(this);
	}
};




PvsLanguageParser.AssignmentExpressionContext = AssignmentExpressionContext;

PvsLanguageParser.prototype.assignmentExpression = function() {

    var localctx = new AssignmentExpressionContext(this, this._ctx, this.state);
    this.enterRule(localctx, 72, PvsLanguageParser.RULE_assignmentExpression);
    var _la = 0; // Token type
    try {
        this.enterOuterAlt(localctx, 1);
        this.state = 606;
        this.identifier();
        this.state = 607;
        _la = this._input.LA(1);
        if(!(_la===PvsLanguageParser.T__13 || _la===PvsLanguageParser.T__14)) {
        this._errHandler.recoverInline(this);
        }
        else {
            this.consume();
        }
        this.state = 608;
        this.expr(0);
    } catch (re) {
    	if(re instanceof antlr4.error.RecognitionException) {
	        localctx.exception = re;
	        this._errHandler.reportError(this, re);
	        this._errHandler.recover(this, re);
	    } else {
	    	throw re;
	    }
    } finally {
        this.exitRule();
    }
    return localctx;
};

function BindingDeclarationContext(parser, parent, invokingState) {
	if(parent===undefined) {
	    parent = null;
	}
	if(invokingState===undefined || invokingState===null) {
		invokingState = -1;
	}
	antlr4.ParserRuleContext.call(this, parent, invokingState);
    this.parser = parser;
    this.ruleIndex = PvsLanguageParser.RULE_bindingDeclaration;
    return this;
}

BindingDeclarationContext.prototype = Object.create(antlr4.ParserRuleContext.prototype);
BindingDeclarationContext.prototype.constructor = BindingDeclarationContext;

BindingDeclarationContext.prototype.identifier = function() {
    return this.getTypedRuleContext(IdentifierContext,0);
};

BindingDeclarationContext.prototype.expr = function() {
    return this.getTypedRuleContext(ExprContext,0);
};

BindingDeclarationContext.prototype.enterRule = function(listener) {
    if(listener instanceof PvsLanguageListener ) {
        listener.enterBindingDeclaration(this);
	}
};

BindingDeclarationContext.prototype.exitRule = function(listener) {
    if(listener instanceof PvsLanguageListener ) {
        listener.exitBindingDeclaration(this);
	}
};




PvsLanguageParser.BindingDeclarationContext = BindingDeclarationContext;

PvsLanguageParser.prototype.bindingDeclaration = function() {

    var localctx = new BindingDeclarationContext(this, this._ctx, this.state);
    this.enterRule(localctx, 74, PvsLanguageParser.RULE_bindingDeclaration);
    try {
        this.enterOuterAlt(localctx, 1);
        this.state = 610;
        this.identifier();
        this.state = 611;
        this.match(PvsLanguageParser.T__3);
        this.state = 612;
        this.expr(0);
    } catch (re) {
    	if(re instanceof antlr4.error.RecognitionException) {
	        localctx.exception = re;
	        this._errHandler.reportError(this, re);
	        this._errHandler.recover(this, re);
	    } else {
	    	throw re;
	    }
    } finally {
        this.exitRule();
    }
    return localctx;
};

function RecordTypeContext(parser, parent, invokingState) {
	if(parent===undefined) {
	    parent = null;
	}
	if(invokingState===undefined || invokingState===null) {
		invokingState = -1;
	}
	antlr4.ParserRuleContext.call(this, parent, invokingState);
    this.parser = parser;
    this.ruleIndex = PvsLanguageParser.RULE_recordType;
    return this;
}

RecordTypeContext.prototype = Object.create(antlr4.ParserRuleContext.prototype);
RecordTypeContext.prototype.constructor = RecordTypeContext;

RecordTypeContext.prototype.bindingDeclaration = function(i) {
    if(i===undefined) {
        i = null;
    }
    if(i===null) {
        return this.getTypedRuleContexts(BindingDeclarationContext);
    } else {
        return this.getTypedRuleContext(BindingDeclarationContext,i);
    }
};

RecordTypeContext.prototype.enterRule = function(listener) {
    if(listener instanceof PvsLanguageListener ) {
        listener.enterRecordType(this);
	}
};

RecordTypeContext.prototype.exitRule = function(listener) {
    if(listener instanceof PvsLanguageListener ) {
        listener.exitRecordType(this);
	}
};




PvsLanguageParser.RecordTypeContext = RecordTypeContext;

PvsLanguageParser.prototype.recordType = function() {

    var localctx = new RecordTypeContext(this, this._ctx, this.state);
    this.enterRule(localctx, 76, PvsLanguageParser.RULE_recordType);
    var _la = 0; // Token type
    try {
        this.enterOuterAlt(localctx, 1);
        this.state = 614;
        this.match(PvsLanguageParser.T__15);
        this.state = 615;
        this.bindingDeclaration();
        this.state = 620;
        this._errHandler.sync(this);
        _la = this._input.LA(1);
        while(_la===PvsLanguageParser.T__1) {
            this.state = 616;
            this.match(PvsLanguageParser.T__1);
            this.state = 617;
            this.bindingDeclaration();
            this.state = 622;
            this._errHandler.sync(this);
            _la = this._input.LA(1);
        }
        this.state = 623;
        this.match(PvsLanguageParser.T__16);
    } catch (re) {
    	if(re instanceof antlr4.error.RecognitionException) {
	        localctx.exception = re;
	        this._errHandler.reportError(this, re);
	        this._errHandler.recover(this, re);
	    } else {
	    	throw re;
	    }
    } finally {
        this.exitRule();
    }
    return localctx;
};

function FunctionTypeContext(parser, parent, invokingState) {
	if(parent===undefined) {
	    parent = null;
	}
	if(invokingState===undefined || invokingState===null) {
		invokingState = -1;
	}
	antlr4.ParserRuleContext.call(this, parent, invokingState);
    this.parser = parser;
    this.ruleIndex = PvsLanguageParser.RULE_functionType;
    return this;
}

FunctionTypeContext.prototype = Object.create(antlr4.ParserRuleContext.prototype);
FunctionTypeContext.prototype.constructor = FunctionTypeContext;

FunctionTypeContext.prototype.typeExpression = function(i) {
    if(i===undefined) {
        i = null;
    }
    if(i===null) {
        return this.getTypedRuleContexts(TypeExpressionContext);
    } else {
        return this.getTypedRuleContext(TypeExpressionContext,i);
    }
};

FunctionTypeContext.prototype.identifierOrOperators = function(i) {
    if(i===undefined) {
        i = null;
    }
    if(i===null) {
        return this.getTypedRuleContexts(IdentifierOrOperatorsContext);
    } else {
        return this.getTypedRuleContext(IdentifierOrOperatorsContext,i);
    }
};

FunctionTypeContext.prototype.K_FUNCTION = function() {
    return this.getToken(PvsLanguageParser.K_FUNCTION, 0);
};

FunctionTypeContext.prototype.K_ARRAY = function() {
    return this.getToken(PvsLanguageParser.K_ARRAY, 0);
};

FunctionTypeContext.prototype.enterRule = function(listener) {
    if(listener instanceof PvsLanguageListener ) {
        listener.enterFunctionType(this);
	}
};

FunctionTypeContext.prototype.exitRule = function(listener) {
    if(listener instanceof PvsLanguageListener ) {
        listener.exitFunctionType(this);
	}
};




PvsLanguageParser.FunctionTypeContext = FunctionTypeContext;

PvsLanguageParser.prototype.functionType = function() {

    var localctx = new FunctionTypeContext(this, this._ctx, this.state);
    this.enterRule(localctx, 78, PvsLanguageParser.RULE_functionType);
    var _la = 0; // Token type
    try {
        this.enterOuterAlt(localctx, 1);
        this.state = 626;
        _la = this._input.LA(1);
        if(_la===PvsLanguageParser.K_FUNCTION || _la===PvsLanguageParser.K_ARRAY) {
            this.state = 625;
            _la = this._input.LA(1);
            if(!(_la===PvsLanguageParser.K_FUNCTION || _la===PvsLanguageParser.K_ARRAY)) {
            this._errHandler.recoverInline(this);
            }
            else {
                this.consume();
            }
        }

        this.state = 628;
        this.match(PvsLanguageParser.T__0);
        this.state = 632;
        var la_ = this._interp.adaptivePredict(this._input,71,this._ctx);
        if(la_===1) {
            this.state = 629;
            this.identifierOrOperators();
            this.state = 630;
            this.match(PvsLanguageParser.T__3);

        }
        this.state = 634;
        this.typeExpression();
        this.state = 644;
        this._errHandler.sync(this);
        _la = this._input.LA(1);
        while(_la===PvsLanguageParser.T__1) {
            this.state = 635;
            this.match(PvsLanguageParser.T__1);
            this.state = 639;
            var la_ = this._interp.adaptivePredict(this._input,72,this._ctx);
            if(la_===1) {
                this.state = 636;
                this.identifierOrOperators();
                this.state = 637;
                this.match(PvsLanguageParser.T__3);

            }
            this.state = 641;
            this.typeExpression();
            this.state = 646;
            this._errHandler.sync(this);
            _la = this._input.LA(1);
        }
        this.state = 647;
        this.match(PvsLanguageParser.T__11);
        this.state = 648;
        this.typeExpression();
        this.state = 649;
        this.match(PvsLanguageParser.T__2);
    } catch (re) {
    	if(re instanceof antlr4.error.RecognitionException) {
	        localctx.exception = re;
	        this._errHandler.reportError(this, re);
	        this._errHandler.recover(this, re);
	    } else {
	    	throw re;
	    }
    } finally {
        this.exitRule();
    }
    return localctx;
};

function TupleTypeContext(parser, parent, invokingState) {
	if(parent===undefined) {
	    parent = null;
	}
	if(invokingState===undefined || invokingState===null) {
		invokingState = -1;
	}
	antlr4.ParserRuleContext.call(this, parent, invokingState);
    this.parser = parser;
    this.ruleIndex = PvsLanguageParser.RULE_tupleType;
    return this;
}

TupleTypeContext.prototype = Object.create(antlr4.ParserRuleContext.prototype);
TupleTypeContext.prototype.constructor = TupleTypeContext;

TupleTypeContext.prototype.typeExpression = function(i) {
    if(i===undefined) {
        i = null;
    }
    if(i===null) {
        return this.getTypedRuleContexts(TypeExpressionContext);
    } else {
        return this.getTypedRuleContext(TypeExpressionContext,i);
    }
};

TupleTypeContext.prototype.identifierOrOperators = function(i) {
    if(i===undefined) {
        i = null;
    }
    if(i===null) {
        return this.getTypedRuleContexts(IdentifierOrOperatorsContext);
    } else {
        return this.getTypedRuleContext(IdentifierOrOperatorsContext,i);
    }
};

TupleTypeContext.prototype.enterRule = function(listener) {
    if(listener instanceof PvsLanguageListener ) {
        listener.enterTupleType(this);
	}
};

TupleTypeContext.prototype.exitRule = function(listener) {
    if(listener instanceof PvsLanguageListener ) {
        listener.exitTupleType(this);
	}
};




PvsLanguageParser.TupleTypeContext = TupleTypeContext;

PvsLanguageParser.prototype.tupleType = function() {

    var localctx = new TupleTypeContext(this, this._ctx, this.state);
    this.enterRule(localctx, 80, PvsLanguageParser.RULE_tupleType);
    var _la = 0; // Token type
    try {
        this.enterOuterAlt(localctx, 1);
        this.state = 651;
        this.match(PvsLanguageParser.T__0);
        this.state = 655;
        var la_ = this._interp.adaptivePredict(this._input,74,this._ctx);
        if(la_===1) {
            this.state = 652;
            this.identifierOrOperators();
            this.state = 653;
            this.match(PvsLanguageParser.T__3);

        }
        this.state = 657;
        this.typeExpression();
        this.state = 667;
        this._errHandler.sync(this);
        _la = this._input.LA(1);
        while(_la===PvsLanguageParser.T__1) {
            this.state = 658;
            this.match(PvsLanguageParser.T__1);
            this.state = 662;
            var la_ = this._interp.adaptivePredict(this._input,75,this._ctx);
            if(la_===1) {
                this.state = 659;
                this.identifierOrOperators();
                this.state = 660;
                this.match(PvsLanguageParser.T__3);

            }
            this.state = 664;
            this.typeExpression();
            this.state = 669;
            this._errHandler.sync(this);
            _la = this._input.LA(1);
        }
        this.state = 670;
        this.match(PvsLanguageParser.T__2);
    } catch (re) {
    	if(re instanceof antlr4.error.RecognitionException) {
	        localctx.exception = re;
	        this._errHandler.reportError(this, re);
	        this._errHandler.recover(this, re);
	    } else {
	    	throw re;
	    }
    } finally {
        this.exitRule();
    }
    return localctx;
};

function SubtypeContext(parser, parent, invokingState) {
	if(parent===undefined) {
	    parent = null;
	}
	if(invokingState===undefined || invokingState===null) {
		invokingState = -1;
	}
	antlr4.ParserRuleContext.call(this, parent, invokingState);
    this.parser = parser;
    this.ruleIndex = PvsLanguageParser.RULE_subtype;
    return this;
}

SubtypeContext.prototype = Object.create(antlr4.ParserRuleContext.prototype);
SubtypeContext.prototype.constructor = SubtypeContext;

SubtypeContext.prototype.identifiers = function() {
    return this.getTypedRuleContext(IdentifiersContext,0);
};

SubtypeContext.prototype.expr = function(i) {
    if(i===undefined) {
        i = null;
    }
    if(i===null) {
        return this.getTypedRuleContexts(ExprContext);
    } else {
        return this.getTypedRuleContext(ExprContext,i);
    }
};

SubtypeContext.prototype.enterRule = function(listener) {
    if(listener instanceof PvsLanguageListener ) {
        listener.enterSubtype(this);
	}
};

SubtypeContext.prototype.exitRule = function(listener) {
    if(listener instanceof PvsLanguageListener ) {
        listener.exitSubtype(this);
	}
};




PvsLanguageParser.SubtypeContext = SubtypeContext;

PvsLanguageParser.prototype.subtype = function() {

    var localctx = new SubtypeContext(this, this._ctx, this.state);
    this.enterRule(localctx, 82, PvsLanguageParser.RULE_subtype);
    var _la = 0; // Token type
    try {
        this.state = 696;
        var la_ = this._interp.adaptivePredict(this._input,79,this._ctx);
        switch(la_) {
        case 1:
            this.enterOuterAlt(localctx, 1);
            this.state = 672;
            this.match(PvsLanguageParser.T__17);
            this.state = 673;
            this.identifiers();
            this.state = 674;
            this.match(PvsLanguageParser.T__3);
            this.state = 675;
            this.expr(0);
            this.state = 676;
            this.match(PvsLanguageParser.T__12);
            this.state = 677;
            this.expr(0);
            this.state = 678;
            this.match(PvsLanguageParser.T__18);
            break;

        case 2:
            this.enterOuterAlt(localctx, 2);
            this.state = 680;
            this.match(PvsLanguageParser.T__4);
            this.state = 681;
            this.identifiers();
            this.state = 688;
            _la = this._input.LA(1);
            if(_la===PvsLanguageParser.T__3) {
                this.state = 682;
                this.match(PvsLanguageParser.T__3);
                this.state = 683;
                this.expr(0);
                this.state = 686;
                _la = this._input.LA(1);
                if(_la===PvsLanguageParser.T__12) {
                    this.state = 684;
                    this.match(PvsLanguageParser.T__12);
                    this.state = 685;
                    this.expr(0);
                }

            }

            this.state = 690;
            this.match(PvsLanguageParser.T__5);
            break;

        case 3:
            this.enterOuterAlt(localctx, 3);
            this.state = 692;
            this.match(PvsLanguageParser.T__4);
            this.state = 693;
            this.expr(0);
            this.state = 694;
            this.match(PvsLanguageParser.T__5);
            break;

        }
    } catch (re) {
    	if(re instanceof antlr4.error.RecognitionException) {
	        localctx.exception = re;
	        this._errHandler.reportError(this, re);
	        this._errHandler.recover(this, re);
	    } else {
	    	throw re;
	    }
    } finally {
        this.exitRule();
    }
    return localctx;
};

function NameContext(parser, parent, invokingState) {
	if(parent===undefined) {
	    parent = null;
	}
	if(invokingState===undefined || invokingState===null) {
		invokingState = -1;
	}
	antlr4.ParserRuleContext.call(this, parent, invokingState);
    this.parser = parser;
    this.ruleIndex = PvsLanguageParser.RULE_name;
    return this;
}

NameContext.prototype = Object.create(antlr4.ParserRuleContext.prototype);
NameContext.prototype.constructor = NameContext;

NameContext.prototype.identifierOrOperator = function() {
    return this.getTypedRuleContext(IdentifierOrOperatorContext,0);
};

NameContext.prototype.identifier = function() {
    return this.getTypedRuleContext(IdentifierContext,0);
};

NameContext.prototype.actuals = function() {
    return this.getTypedRuleContext(ActualsContext,0);
};

NameContext.prototype.arguments = function(i) {
    if(i===undefined) {
        i = null;
    }
    if(i===null) {
        return this.getTypedRuleContexts(ArgumentsContext);
    } else {
        return this.getTypedRuleContext(ArgumentsContext,i);
    }
};

NameContext.prototype.enterRule = function(listener) {
    if(listener instanceof PvsLanguageListener ) {
        listener.enterName(this);
	}
};

NameContext.prototype.exitRule = function(listener) {
    if(listener instanceof PvsLanguageListener ) {
        listener.exitName(this);
	}
};




PvsLanguageParser.NameContext = NameContext;

PvsLanguageParser.prototype.name = function() {

    var localctx = new NameContext(this, this._ctx, this.state);
    this.enterRule(localctx, 84, PvsLanguageParser.RULE_name);
    try {
        this.enterOuterAlt(localctx, 1);
        this.state = 701;
        var la_ = this._interp.adaptivePredict(this._input,80,this._ctx);
        if(la_===1) {
            this.state = 698;
            this.identifier();
            this.state = 699;
            this.match(PvsLanguageParser.T__19);

        }
        this.state = 703;
        this.identifierOrOperator();
        this.state = 705;
        var la_ = this._interp.adaptivePredict(this._input,81,this._ctx);
        if(la_===1) {
            this.state = 704;
            this.actuals();

        }
        this.state = 710;
        this._errHandler.sync(this);
        var _alt = this._interp.adaptivePredict(this._input,82,this._ctx)
        while(_alt!=2 && _alt!=antlr4.atn.ATN.INVALID_ALT_NUMBER) {
            if(_alt===1) {
                this.state = 707;
                this.arguments(); 
            }
            this.state = 712;
            this._errHandler.sync(this);
            _alt = this._interp.adaptivePredict(this._input,82,this._ctx);
        }

    } catch (re) {
    	if(re instanceof antlr4.error.RecognitionException) {
	        localctx.exception = re;
	        this._errHandler.reportError(this, re);
	        this._errHandler.recover(this, re);
	    } else {
	    	throw re;
	    }
    } finally {
        this.exitRule();
    }
    return localctx;
};

function ActualsContext(parser, parent, invokingState) {
	if(parent===undefined) {
	    parent = null;
	}
	if(invokingState===undefined || invokingState===null) {
		invokingState = -1;
	}
	antlr4.ParserRuleContext.call(this, parent, invokingState);
    this.parser = parser;
    this.ruleIndex = PvsLanguageParser.RULE_actuals;
    return this;
}

ActualsContext.prototype = Object.create(antlr4.ParserRuleContext.prototype);
ActualsContext.prototype.constructor = ActualsContext;

ActualsContext.prototype.expr = function(i) {
    if(i===undefined) {
        i = null;
    }
    if(i===null) {
        return this.getTypedRuleContexts(ExprContext);
    } else {
        return this.getTypedRuleContext(ExprContext,i);
    }
};

ActualsContext.prototype.enterRule = function(listener) {
    if(listener instanceof PvsLanguageListener ) {
        listener.enterActuals(this);
	}
};

ActualsContext.prototype.exitRule = function(listener) {
    if(listener instanceof PvsLanguageListener ) {
        listener.exitActuals(this);
	}
};




PvsLanguageParser.ActualsContext = ActualsContext;

PvsLanguageParser.prototype.actuals = function() {

    var localctx = new ActualsContext(this, this._ctx, this.state);
    this.enterRule(localctx, 86, PvsLanguageParser.RULE_actuals);
    var _la = 0; // Token type
    try {
        this.enterOuterAlt(localctx, 1);
        this.state = 713;
        this.match(PvsLanguageParser.T__0);
        this.state = 714;
        this.expr(0);
        this.state = 719;
        this._errHandler.sync(this);
        _la = this._input.LA(1);
        while(_la===PvsLanguageParser.T__1) {
            this.state = 715;
            this.match(PvsLanguageParser.T__1);
            this.state = 716;
            this.expr(0);
            this.state = 721;
            this._errHandler.sync(this);
            _la = this._input.LA(1);
        }
        this.state = 722;
        this.match(PvsLanguageParser.T__2);
    } catch (re) {
    	if(re instanceof antlr4.error.RecognitionException) {
	        localctx.exception = re;
	        this._errHandler.reportError(this, re);
	        this._errHandler.recover(this, re);
	    } else {
	    	throw re;
	    }
    } finally {
        this.exitRule();
    }
    return localctx;
};

function EnumerationTypeContext(parser, parent, invokingState) {
	if(parent===undefined) {
	    parent = null;
	}
	if(invokingState===undefined || invokingState===null) {
		invokingState = -1;
	}
	antlr4.ParserRuleContext.call(this, parent, invokingState);
    this.parser = parser;
    this.ruleIndex = PvsLanguageParser.RULE_enumerationType;
    return this;
}

EnumerationTypeContext.prototype = Object.create(antlr4.ParserRuleContext.prototype);
EnumerationTypeContext.prototype.constructor = EnumerationTypeContext;

EnumerationTypeContext.prototype.identifierOrOperators = function() {
    return this.getTypedRuleContext(IdentifierOrOperatorsContext,0);
};

EnumerationTypeContext.prototype.enterRule = function(listener) {
    if(listener instanceof PvsLanguageListener ) {
        listener.enterEnumerationType(this);
	}
};

EnumerationTypeContext.prototype.exitRule = function(listener) {
    if(listener instanceof PvsLanguageListener ) {
        listener.exitEnumerationType(this);
	}
};




PvsLanguageParser.EnumerationTypeContext = EnumerationTypeContext;

PvsLanguageParser.prototype.enumerationType = function() {

    var localctx = new EnumerationTypeContext(this, this._ctx, this.state);
    this.enterRule(localctx, 88, PvsLanguageParser.RULE_enumerationType);
    try {
        this.enterOuterAlt(localctx, 1);
        this.state = 724;
        this.match(PvsLanguageParser.T__17);
        this.state = 725;
        this.identifierOrOperators();
        this.state = 726;
        this.match(PvsLanguageParser.T__18);
    } catch (re) {
    	if(re instanceof antlr4.error.RecognitionException) {
	        localctx.exception = re;
	        this._errHandler.reportError(this, re);
	        this._errHandler.recover(this, re);
	    } else {
	    	throw re;
	    }
    } finally {
        this.exitRule();
    }
    return localctx;
};

function ImportingContext(parser, parent, invokingState) {
	if(parent===undefined) {
	    parent = null;
	}
	if(invokingState===undefined || invokingState===null) {
		invokingState = -1;
	}
	antlr4.ParserRuleContext.call(this, parent, invokingState);
    this.parser = parser;
    this.ruleIndex = PvsLanguageParser.RULE_importing;
    return this;
}

ImportingContext.prototype = Object.create(antlr4.ParserRuleContext.prototype);
ImportingContext.prototype.constructor = ImportingContext;

ImportingContext.prototype.K_IMPORTING = function() {
    return this.getToken(PvsLanguageParser.K_IMPORTING, 0);
};

ImportingContext.prototype.theoryName = function(i) {
    if(i===undefined) {
        i = null;
    }
    if(i===null) {
        return this.getTypedRuleContexts(TheoryNameContext);
    } else {
        return this.getTypedRuleContext(TheoryNameContext,i);
    }
};

ImportingContext.prototype.enterRule = function(listener) {
    if(listener instanceof PvsLanguageListener ) {
        listener.enterImporting(this);
	}
};

ImportingContext.prototype.exitRule = function(listener) {
    if(listener instanceof PvsLanguageListener ) {
        listener.exitImporting(this);
	}
};




PvsLanguageParser.ImportingContext = ImportingContext;

PvsLanguageParser.prototype.importing = function() {

    var localctx = new ImportingContext(this, this._ctx, this.state);
    this.enterRule(localctx, 90, PvsLanguageParser.RULE_importing);
    var _la = 0; // Token type
    try {
        this.enterOuterAlt(localctx, 1);
        this.state = 728;
        this.match(PvsLanguageParser.K_IMPORTING);
        this.state = 729;
        this.theoryName();
        this.state = 734;
        this._errHandler.sync(this);
        _la = this._input.LA(1);
        while(_la===PvsLanguageParser.T__1) {
            this.state = 730;
            this.match(PvsLanguageParser.T__1);
            this.state = 731;
            this.theoryName();
            this.state = 736;
            this._errHandler.sync(this);
            _la = this._input.LA(1);
        }
    } catch (re) {
    	if(re instanceof antlr4.error.RecognitionException) {
	        localctx.exception = re;
	        this._errHandler.reportError(this, re);
	        this._errHandler.recover(this, re);
	    } else {
	    	throw re;
	    }
    } finally {
        this.exitRule();
    }
    return localctx;
};

function TheoryNameContext(parser, parent, invokingState) {
	if(parent===undefined) {
	    parent = null;
	}
	if(invokingState===undefined || invokingState===null) {
		invokingState = -1;
	}
	antlr4.ParserRuleContext.call(this, parent, invokingState);
    this.parser = parser;
    this.ruleIndex = PvsLanguageParser.RULE_theoryName;
    return this;
}

TheoryNameContext.prototype = Object.create(antlr4.ParserRuleContext.prototype);
TheoryNameContext.prototype.constructor = TheoryNameContext;

TheoryNameContext.prototype.identifier = function(i) {
    if(i===undefined) {
        i = null;
    }
    if(i===null) {
        return this.getTypedRuleContexts(IdentifierContext);
    } else {
        return this.getTypedRuleContext(IdentifierContext,i);
    }
};

TheoryNameContext.prototype.actuals = function() {
    return this.getTypedRuleContext(ActualsContext,0);
};

TheoryNameContext.prototype.enterRule = function(listener) {
    if(listener instanceof PvsLanguageListener ) {
        listener.enterTheoryName(this);
	}
};

TheoryNameContext.prototype.exitRule = function(listener) {
    if(listener instanceof PvsLanguageListener ) {
        listener.exitTheoryName(this);
	}
};




PvsLanguageParser.TheoryNameContext = TheoryNameContext;

PvsLanguageParser.prototype.theoryName = function() {

    var localctx = new TheoryNameContext(this, this._ctx, this.state);
    this.enterRule(localctx, 92, PvsLanguageParser.RULE_theoryName);
    var _la = 0; // Token type
    try {
        this.enterOuterAlt(localctx, 1);
        this.state = 740;
        var la_ = this._interp.adaptivePredict(this._input,85,this._ctx);
        if(la_===1) {
            this.state = 737;
            this.identifier();
            this.state = 738;
            this.match(PvsLanguageParser.T__19);

        }
        this.state = 742;
        this.identifier();
        this.state = 744;
        _la = this._input.LA(1);
        if(_la===PvsLanguageParser.T__0) {
            this.state = 743;
            this.actuals();
        }

    } catch (re) {
    	if(re instanceof antlr4.error.RecognitionException) {
	        localctx.exception = re;
	        this._errHandler.reportError(this, re);
	        this._errHandler.recover(this, re);
	    } else {
	    	throw re;
	    }
    } finally {
        this.exitRule();
    }
    return localctx;
};

function DatatypeContext(parser, parent, invokingState) {
	if(parent===undefined) {
	    parent = null;
	}
	if(invokingState===undefined || invokingState===null) {
		invokingState = -1;
	}
	antlr4.ParserRuleContext.call(this, parent, invokingState);
    this.parser = parser;
    this.ruleIndex = PvsLanguageParser.RULE_datatype;
    return this;
}

DatatypeContext.prototype = Object.create(antlr4.ParserRuleContext.prototype);
DatatypeContext.prototype.constructor = DatatypeContext;

DatatypeContext.prototype.identifier = function(i) {
    if(i===undefined) {
        i = null;
    }
    if(i===null) {
        return this.getTypedRuleContexts(IdentifierContext);
    } else {
        return this.getTypedRuleContext(IdentifierContext,i);
    }
};

DatatypeContext.prototype.K_DATATYPE = function() {
    return this.getToken(PvsLanguageParser.K_DATATYPE, 0);
};

DatatypeContext.prototype.K_BEGIN = function() {
    return this.getToken(PvsLanguageParser.K_BEGIN, 0);
};

DatatypeContext.prototype.K_END = function() {
    return this.getToken(PvsLanguageParser.K_END, 0);
};

DatatypeContext.prototype.enterRule = function(listener) {
    if(listener instanceof PvsLanguageListener ) {
        listener.enterDatatype(this);
	}
};

DatatypeContext.prototype.exitRule = function(listener) {
    if(listener instanceof PvsLanguageListener ) {
        listener.exitDatatype(this);
	}
};




PvsLanguageParser.DatatypeContext = DatatypeContext;

PvsLanguageParser.prototype.datatype = function() {

    var localctx = new DatatypeContext(this, this._ctx, this.state);
    this.enterRule(localctx, 94, PvsLanguageParser.RULE_datatype);
    try {
        this.enterOuterAlt(localctx, 1);
        this.state = 746;
        this.identifier();
        this.state = 747;
        this.match(PvsLanguageParser.T__3);
        this.state = 748;
        this.match(PvsLanguageParser.K_DATATYPE);
        this.state = 749;
        this.match(PvsLanguageParser.K_BEGIN);
        this.state = 750;
        this.match(PvsLanguageParser.K_END);
        this.state = 751;
        this.identifier();
    } catch (re) {
    	if(re instanceof antlr4.error.RecognitionException) {
	        localctx.exception = re;
	        this._errHandler.reportError(this, re);
	        this._errHandler.recover(this, re);
	    } else {
	    	throw re;
	    }
    } finally {
        this.exitRule();
    }
    return localctx;
};

function IdentifierContext(parser, parent, invokingState) {
	if(parent===undefined) {
	    parent = null;
	}
	if(invokingState===undefined || invokingState===null) {
		invokingState = -1;
	}
	antlr4.ParserRuleContext.call(this, parent, invokingState);
    this.parser = parser;
    this.ruleIndex = PvsLanguageParser.RULE_identifier;
    return this;
}

IdentifierContext.prototype = Object.create(antlr4.ParserRuleContext.prototype);
IdentifierContext.prototype.constructor = IdentifierContext;

IdentifierContext.prototype.ID = function() {
    return this.getToken(PvsLanguageParser.ID, 0);
};

IdentifierContext.prototype.enterRule = function(listener) {
    if(listener instanceof PvsLanguageListener ) {
        listener.enterIdentifier(this);
	}
};

IdentifierContext.prototype.exitRule = function(listener) {
    if(listener instanceof PvsLanguageListener ) {
        listener.exitIdentifier(this);
	}
};




PvsLanguageParser.IdentifierContext = IdentifierContext;

PvsLanguageParser.prototype.identifier = function() {

    var localctx = new IdentifierContext(this, this._ctx, this.state);
    this.enterRule(localctx, 96, PvsLanguageParser.RULE_identifier);
    try {
        this.enterOuterAlt(localctx, 1);
        this.state = 753;
        this.match(PvsLanguageParser.ID);
    } catch (re) {
    	if(re instanceof antlr4.error.RecognitionException) {
	        localctx.exception = re;
	        this._errHandler.reportError(this, re);
	        this._errHandler.recover(this, re);
	    } else {
	    	throw re;
	    }
    } finally {
        this.exitRule();
    }
    return localctx;
};

function IdentifiersContext(parser, parent, invokingState) {
	if(parent===undefined) {
	    parent = null;
	}
	if(invokingState===undefined || invokingState===null) {
		invokingState = -1;
	}
	antlr4.ParserRuleContext.call(this, parent, invokingState);
    this.parser = parser;
    this.ruleIndex = PvsLanguageParser.RULE_identifiers;
    return this;
}

IdentifiersContext.prototype = Object.create(antlr4.ParserRuleContext.prototype);
IdentifiersContext.prototype.constructor = IdentifiersContext;

IdentifiersContext.prototype.identifier = function(i) {
    if(i===undefined) {
        i = null;
    }
    if(i===null) {
        return this.getTypedRuleContexts(IdentifierContext);
    } else {
        return this.getTypedRuleContext(IdentifierContext,i);
    }
};

IdentifiersContext.prototype.enterRule = function(listener) {
    if(listener instanceof PvsLanguageListener ) {
        listener.enterIdentifiers(this);
	}
};

IdentifiersContext.prototype.exitRule = function(listener) {
    if(listener instanceof PvsLanguageListener ) {
        listener.exitIdentifiers(this);
	}
};




PvsLanguageParser.IdentifiersContext = IdentifiersContext;

PvsLanguageParser.prototype.identifiers = function() {

    var localctx = new IdentifiersContext(this, this._ctx, this.state);
    this.enterRule(localctx, 98, PvsLanguageParser.RULE_identifiers);
    try {
        this.enterOuterAlt(localctx, 1);
        this.state = 755;
        this.identifier();
        this.state = 760;
        this._errHandler.sync(this);
        var _alt = this._interp.adaptivePredict(this._input,87,this._ctx)
        while(_alt!=2 && _alt!=antlr4.atn.ATN.INVALID_ALT_NUMBER) {
            if(_alt===1) {
                this.state = 756;
                this.match(PvsLanguageParser.T__1);
                this.state = 757;
                this.identifier(); 
            }
            this.state = 762;
            this._errHandler.sync(this);
            _alt = this._interp.adaptivePredict(this._input,87,this._ctx);
        }

    } catch (re) {
    	if(re instanceof antlr4.error.RecognitionException) {
	        localctx.exception = re;
	        this._errHandler.reportError(this, re);
	        this._errHandler.recover(this, re);
	    } else {
	    	throw re;
	    }
    } finally {
        this.exitRule();
    }
    return localctx;
};

function IdentifierOrOperatorContext(parser, parent, invokingState) {
	if(parent===undefined) {
	    parent = null;
	}
	if(invokingState===undefined || invokingState===null) {
		invokingState = -1;
	}
	antlr4.ParserRuleContext.call(this, parent, invokingState);
    this.parser = parser;
    this.ruleIndex = PvsLanguageParser.RULE_identifierOrOperator;
    return this;
}

IdentifierOrOperatorContext.prototype = Object.create(antlr4.ParserRuleContext.prototype);
IdentifierOrOperatorContext.prototype.constructor = IdentifierOrOperatorContext;

IdentifierOrOperatorContext.prototype.identifier = function() {
    return this.getTypedRuleContext(IdentifierContext,0);
};

IdentifierOrOperatorContext.prototype.unaryOp = function() {
    return this.getTypedRuleContext(UnaryOpContext,0);
};

IdentifierOrOperatorContext.prototype.binaryOp = function() {
    return this.getTypedRuleContext(BinaryOpContext,0);
};

IdentifierOrOperatorContext.prototype.enterRule = function(listener) {
    if(listener instanceof PvsLanguageListener ) {
        listener.enterIdentifierOrOperator(this);
	}
};

IdentifierOrOperatorContext.prototype.exitRule = function(listener) {
    if(listener instanceof PvsLanguageListener ) {
        listener.exitIdentifierOrOperator(this);
	}
};




PvsLanguageParser.IdentifierOrOperatorContext = IdentifierOrOperatorContext;

PvsLanguageParser.prototype.identifierOrOperator = function() {

    var localctx = new IdentifierOrOperatorContext(this, this._ctx, this.state);
    this.enterRule(localctx, 100, PvsLanguageParser.RULE_identifierOrOperator);
    try {
        this.state = 766;
        var la_ = this._interp.adaptivePredict(this._input,88,this._ctx);
        switch(la_) {
        case 1:
            this.enterOuterAlt(localctx, 1);
            this.state = 763;
            this.identifier();
            break;

        case 2:
            this.enterOuterAlt(localctx, 2);
            this.state = 764;
            this.unaryOp();
            break;

        case 3:
            this.enterOuterAlt(localctx, 3);
            this.state = 765;
            this.binaryOp();
            break;

        }
    } catch (re) {
    	if(re instanceof antlr4.error.RecognitionException) {
	        localctx.exception = re;
	        this._errHandler.reportError(this, re);
	        this._errHandler.recover(this, re);
	    } else {
	    	throw re;
	    }
    } finally {
        this.exitRule();
    }
    return localctx;
};

function IdentifierOrOperatorsContext(parser, parent, invokingState) {
	if(parent===undefined) {
	    parent = null;
	}
	if(invokingState===undefined || invokingState===null) {
		invokingState = -1;
	}
	antlr4.ParserRuleContext.call(this, parent, invokingState);
    this.parser = parser;
    this.ruleIndex = PvsLanguageParser.RULE_identifierOrOperators;
    return this;
}

IdentifierOrOperatorsContext.prototype = Object.create(antlr4.ParserRuleContext.prototype);
IdentifierOrOperatorsContext.prototype.constructor = IdentifierOrOperatorsContext;

IdentifierOrOperatorsContext.prototype.identifierOrOperator = function(i) {
    if(i===undefined) {
        i = null;
    }
    if(i===null) {
        return this.getTypedRuleContexts(IdentifierOrOperatorContext);
    } else {
        return this.getTypedRuleContext(IdentifierOrOperatorContext,i);
    }
};

IdentifierOrOperatorsContext.prototype.enterRule = function(listener) {
    if(listener instanceof PvsLanguageListener ) {
        listener.enterIdentifierOrOperators(this);
	}
};

IdentifierOrOperatorsContext.prototype.exitRule = function(listener) {
    if(listener instanceof PvsLanguageListener ) {
        listener.exitIdentifierOrOperators(this);
	}
};




PvsLanguageParser.IdentifierOrOperatorsContext = IdentifierOrOperatorsContext;

PvsLanguageParser.prototype.identifierOrOperators = function() {

    var localctx = new IdentifierOrOperatorsContext(this, this._ctx, this.state);
    this.enterRule(localctx, 102, PvsLanguageParser.RULE_identifierOrOperators);
    var _la = 0; // Token type
    try {
        this.enterOuterAlt(localctx, 1);
        this.state = 768;
        this.identifierOrOperator();
        this.state = 773;
        this._errHandler.sync(this);
        _la = this._input.LA(1);
        while(_la===PvsLanguageParser.T__1) {
            this.state = 769;
            this.match(PvsLanguageParser.T__1);
            this.state = 770;
            this.identifierOrOperator();
            this.state = 775;
            this._errHandler.sync(this);
            _la = this._input.LA(1);
        }
    } catch (re) {
    	if(re instanceof antlr4.error.RecognitionException) {
	        localctx.exception = re;
	        this._errHandler.reportError(this, re);
	        this._errHandler.recover(this, re);
	    } else {
	    	throw re;
	    }
    } finally {
        this.exitRule();
    }
    return localctx;
};

function UnaryOpContext(parser, parent, invokingState) {
	if(parent===undefined) {
	    parent = null;
	}
	if(invokingState===undefined || invokingState===null) {
		invokingState = -1;
	}
	antlr4.ParserRuleContext.call(this, parent, invokingState);
    this.parser = parser;
    this.ruleIndex = PvsLanguageParser.RULE_unaryOp;
    return this;
}

UnaryOpContext.prototype = Object.create(antlr4.ParserRuleContext.prototype);
UnaryOpContext.prototype.constructor = UnaryOpContext;

UnaryOpContext.prototype.O_NOT = function() {
    return this.getToken(PvsLanguageParser.O_NOT, 0);
};

UnaryOpContext.prototype.enterRule = function(listener) {
    if(listener instanceof PvsLanguageListener ) {
        listener.enterUnaryOp(this);
	}
};

UnaryOpContext.prototype.exitRule = function(listener) {
    if(listener instanceof PvsLanguageListener ) {
        listener.exitUnaryOp(this);
	}
};




PvsLanguageParser.UnaryOpContext = UnaryOpContext;

PvsLanguageParser.prototype.unaryOp = function() {

    var localctx = new UnaryOpContext(this, this._ctx, this.state);
    this.enterRule(localctx, 104, PvsLanguageParser.RULE_unaryOp);
    var _la = 0; // Token type
    try {
        this.enterOuterAlt(localctx, 1);
        this.state = 776;
        _la = this._input.LA(1);
        if(!((((_la) & ~0x1f) == 0 && ((1 << _la) & ((1 << PvsLanguageParser.T__20) | (1 << PvsLanguageParser.T__21) | (1 << PvsLanguageParser.T__22) | (1 << PvsLanguageParser.T__23) | (1 << PvsLanguageParser.T__24))) !== 0) || _la===PvsLanguageParser.O_NOT)) {
        this._errHandler.recoverInline(this);
        }
        else {
            this.consume();
        }
    } catch (re) {
    	if(re instanceof antlr4.error.RecognitionException) {
	        localctx.exception = re;
	        this._errHandler.reportError(this, re);
	        this._errHandler.recover(this, re);
	    } else {
	    	throw re;
	    }
    } finally {
        this.exitRule();
    }
    return localctx;
};

function BinaryOpContext(parser, parent, invokingState) {
	if(parent===undefined) {
	    parent = null;
	}
	if(invokingState===undefined || invokingState===null) {
		invokingState = -1;
	}
	antlr4.ParserRuleContext.call(this, parent, invokingState);
    this.parser = parser;
    this.ruleIndex = PvsLanguageParser.RULE_binaryOp;
    return this;
}

BinaryOpContext.prototype = Object.create(antlr4.ParserRuleContext.prototype);
BinaryOpContext.prototype.constructor = BinaryOpContext;

BinaryOpContext.prototype.O_CONCAT = function() {
    return this.getToken(PvsLanguageParser.O_CONCAT, 0);
};

BinaryOpContext.prototype.O_IFF = function() {
    return this.getToken(PvsLanguageParser.O_IFF, 0);
};

BinaryOpContext.prototype.O_IMPLIES = function() {
    return this.getToken(PvsLanguageParser.O_IMPLIES, 0);
};

BinaryOpContext.prototype.O_AND = function() {
    return this.getToken(PvsLanguageParser.O_AND, 0);
};

BinaryOpContext.prototype.O_OR = function() {
    return this.getToken(PvsLanguageParser.O_OR, 0);
};

BinaryOpContext.prototype.O_LE = function() {
    return this.getToken(PvsLanguageParser.O_LE, 0);
};

BinaryOpContext.prototype.O_GE = function() {
    return this.getToken(PvsLanguageParser.O_GE, 0);
};

BinaryOpContext.prototype.O_NOT_EQUAL = function() {
    return this.getToken(PvsLanguageParser.O_NOT_EQUAL, 0);
};

BinaryOpContext.prototype.O_EQUAL = function() {
    return this.getToken(PvsLanguageParser.O_EQUAL, 0);
};

BinaryOpContext.prototype.O_EXP = function() {
    return this.getToken(PvsLanguageParser.O_EXP, 0);
};

BinaryOpContext.prototype.enterRule = function(listener) {
    if(listener instanceof PvsLanguageListener ) {
        listener.enterBinaryOp(this);
	}
};

BinaryOpContext.prototype.exitRule = function(listener) {
    if(listener instanceof PvsLanguageListener ) {
        listener.exitBinaryOp(this);
	}
};




PvsLanguageParser.BinaryOpContext = BinaryOpContext;

PvsLanguageParser.prototype.binaryOp = function() {

    var localctx = new BinaryOpContext(this, this._ctx, this.state);
    this.enterRule(localctx, 106, PvsLanguageParser.RULE_binaryOp);
    var _la = 0; // Token type
    try {
        this.enterOuterAlt(localctx, 1);
        this.state = 778;
        _la = this._input.LA(1);
        if(!((((_la) & ~0x1f) == 0 && ((1 << _la) & ((1 << PvsLanguageParser.T__20) | (1 << PvsLanguageParser.T__21) | (1 << PvsLanguageParser.T__25) | (1 << PvsLanguageParser.T__26) | (1 << PvsLanguageParser.T__27) | (1 << PvsLanguageParser.T__28) | (1 << PvsLanguageParser.T__29))) !== 0) || ((((_la - 70)) & ~0x1f) == 0 && ((1 << (_la - 70)) & ((1 << (PvsLanguageParser.O_IFF - 70)) | (1 << (PvsLanguageParser.O_IMPLIES - 70)) | (1 << (PvsLanguageParser.O_AND - 70)) | (1 << (PvsLanguageParser.O_OR - 70)) | (1 << (PvsLanguageParser.O_NOT_EQUAL - 70)) | (1 << (PvsLanguageParser.O_LE - 70)) | (1 << (PvsLanguageParser.O_GE - 70)) | (1 << (PvsLanguageParser.O_EQUAL - 70)) | (1 << (PvsLanguageParser.O_EXP - 70)) | (1 << (PvsLanguageParser.O_CONCAT - 70)))) !== 0))) {
        this._errHandler.recoverInline(this);
        }
        else {
            this.consume();
        }
    } catch (re) {
    	if(re instanceof antlr4.error.RecognitionException) {
	        localctx.exception = re;
	        this._errHandler.reportError(this, re);
	        this._errHandler.recover(this, re);
	    } else {
	    	throw re;
	    }
    } finally {
        this.exitRule();
    }
    return localctx;
};


PvsLanguageParser.prototype.sempred = function(localctx, ruleIndex, predIndex) {
	switch(ruleIndex) {
	case 22:
			return this.expr_sempred(localctx, predIndex);
    default:
        throw "No predicate with index:" + ruleIndex;
   }
};

PvsLanguageParser.prototype.expr_sempred = function(localctx, predIndex) {
	switch(predIndex) {
		case 0:
			return this.precpred(this._ctx, 17);
		case 1:
			return this.precpred(this._ctx, 16);
		case 2:
			return this.precpred(this._ctx, 4);
		case 3:
			return this.precpred(this._ctx, 3);
		default:
			throw "No predicate with index:" + predIndex;
	}
};


exports.PvsLanguageParser = PvsLanguageParser;
