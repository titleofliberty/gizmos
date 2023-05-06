unit htmlcolors;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics;

type
  THtmlColor = record
    Name : String;
    Color: TColor;
  end;

const
  clHTMLAliceBlue            = TColor($FFF8F0);
  clHTMLAntiqueWhite         = TColor($D7EBFA);
  clHTMLAqua                 = TColor($FFFF00);
  clHTMLAquamarine           = TColor($D4FF7F);
  clHTMLAzure                = TColor($FFFFF0);
  clHTMLBeige                = TColor($DCF5F5);
  clHTMLBisque               = TColor($C4E4FF);
  clHTMLBlack                = TColor($000000);
  clHTMLBlanchedAlmond       = TColor($CDEBFF);
  clHTMLBlue                 = TColor($FF0000);
  clHTMLBlueViolet           = TColor($E22B8A);
  clHTMLBrown                = TColor($2A2AA5);
  clHTMLBurlyWood            = TColor($87B8DE);
  clHTMLCadetBlue            = TColor($A09E5F);
  clHTMLChartreuse           = TColor($00FF7F);
  clHTMLChocolate            = TColor($1E69D2);
  clHTMLCoral                = TColor($507FFF);
  clHTMLCornflowerBlue       = TColor($ED9564);
  clHTMLCornsilk             = TColor($DCF8FF);
  clHTMLCrimson              = TColor($3C14DC);
  clHTMLCyan                 = TColor($FFFF00);
  clHTMLDarkBlue             = TColor($8B0000);
  clHTMLDarkCyan             = TColor($8B8B00);
  clHTMLDarkGoldenRod        = TColor($0B86B8);
  clHTMLDarkGray             = TColor($A9A9A9);
  clHTMLDarkGreen            = TColor($006400);
  clHTMLDarkKhaki            = TColor($6BB7BD);
  clHTMLDarkMagenta          = TColor($8B008B);
  clHTMLDarkOliveGreen       = TColor($2F6B55);
  clHTMLDarkOrange           = TColor($008CFF);
  clHTMLDarkOrchid           = TColor($CC3299);
  clHTMLDarkRed              = TColor($00008B);
  clHTMLDarkSalmon           = TColor($7A96E9);
  clHTMLDarkSeaGreen         = TColor($8FBC8F);
  clHTMLDarkSlateBlue        = TColor($8B3D48);
  clHTMLDarkSlateGray        = TColor($4F4F2F);
  clHTMLDarkTurquoise        = TColor($D1CE00);
  clHTMLDarkViolet           = TColor($D30094);
  clHTMLDeepPink             = TColor($9314FF);
  clHTMLDeepSkyBlue          = TColor($FFBF00);
  clHTMLDimGray              = TColor($696969);
  clHTMLDodgerBlue           = TColor($FF901E);
  clHTMLFireBrick            = TColor($2222B2);
  clHTMLFloralWhite          = TColor($F0FAFF);
  clHTMLForestGreen          = TColor($228B22);
  clHTMLFuchsia              = TColor($FF00FF);
  clHTMLGainsboro            = TColor($DCDCDC);
  clHTMLGhostWhite           = TColor($FFF8F8);
  clHTMLGold                 = TColor($00D7FF);
  clHTMLGoldenRod            = TColor($20A5DA);
  clHTMLGray                 = TColor($808080);
  clHTMLGreen                = TColor($008000);
  clHTMLGreenYellow          = TColor($2FFFAD);
  clHTMLGrey                 = TColor($808080);
  clHTMLHoneyDew             = TColor($F0FFF0);
  clHTMLHotPink              = TColor($B469FF);
  clHTMLIndianRed            = TColor($5C5CCD);
  clHTMLIndigo               = TColor($82004B);
  clHTMLIvory                = TColor($F0FFFF);
  clHTMLKhaki                = TColor($8CE6F0);
  clHTMLLavender             = TColor($FAE6E6);
  clHTMLLavenderBlush        = TColor($F5F0FF);
  clHTMLLawnGreen            = TColor($00FC7C);
  clHTMLLemonChiffon         = TColor($CDFAFF);
  clHTMLLightBlue            = TColor($E6D8AD);
  clHTMLLightCoral           = TColor($8080F0);
  clHTMLLightCyan            = TColor($FFFFE0);
  clHTMLLightGoldenRodYellow = TColor($D2FAFA);
  clHTMLLightGray            = TColor($D3D3D3);
  clHTMLLightGreen           = TColor($90EE90);
  clHTMLLightGrey            = TColor($D3D3D3);
  clHTMLLightPink            = TColor($C1B6FF);
  clHTMLLightSalmon          = TColor($7AA0FF);
  clHTMLLightSeaGreen        = TColor($AAB220);
  clHTMLLightSkyBlue         = TColor($FACE87);
  clHTMLLightSlateGray       = TColor($998877);
  clHTMLLightSteelBlue       = TColor($DEC4B0);
  clHTMLLightYellow          = TColor($E0FFFF);
  clHTMLLime                 = TColor($00FF00);
  clHTMLLimeGreen            = TColor($32CD32);
  clHTMLLinen                = TColor($E6F0FA);
  clHTMLMagenta              = TColor($FF00FF);
  clHTMLMaroon               = TColor($000080);
  clHTMLMediumAquaMarine     = TColor($AACD66);
  clHTMLMediumBlue           = TColor($CD0000);
  clHTMLMediumOrchid         = TColor($D355BA);
  clHTMLMediumPurple         = TColor($DB7093);
  clHTMLMediumSeaGreen       = TColor($71B33C);
  clHTMLMediumSlateBlue      = TColor($EE687B);
  clHTMLMediumSpringGreen    = TColor($9AFA00);
  clHTMLMediumTurquoise      = TColor($CCD148);
  clHTMLMediumVioletRed      = TColor($8515C7);
  clHTMLMidnightBlue         = TColor($701919);
  clHTMLMintCream            = TColor($FAFFF5);
  clHTMLMistyRose            = TColor($E1E4FF);
  clHTMLMoccasin             = TColor($B5E4FF);
  clHTMLNavajoWhite          = TColor($ADDEFF);
  clHTMLNavy                 = TColor($800000);
  clHTMLOldLace              = TColor($E6F5FD);
  clHTMLOlive                = TColor($008080);
  clHTMLOliveDrab            = TColor($238E6B);
  clHTMLOrange               = TColor($00A5FF);
  clHTMLOrangeRed            = TColor($0045FF);
  clHTMLOrchid               = TColor($D670DA);
  clHTMLPaleGoldenRod        = TColor($AAE8EE);
  clHTMLPaleGreen            = TColor($98FB98);
  clHTMLPaleTurquoise        = TColor($EEEEAF);
  clHTMLPaleVioletRed        = TColor($9370DB);
  clHTMLPapayaWhip           = TColor($D5EFFF);
  clHTMLPeachPuff            = TColor($B9DAFF);
  clHTMLPeru                 = TColor($3F85CD);
  clHTMLPink                 = TColor($CBC0FF);
  clHTMLPlum                 = TColor($DDA0DD);
  clHTMLPowderBlue           = TColor($E6E0B0);
  clHTMLPurple               = TColor($800080);
  clHTMLRebeccaPurple        = TColor($993366);
  clHTMLRed                  = TColor($0000FF);
  clHTMLRosyBrown            = TColor($8F8FBC);
  clHTMLRoyalBlue            = TColor($E16941);
  clHTMLSaddleBrown          = TColor($13458B);
  clHTMLSalmon               = TColor($7280FA);
  clHTMLSandyBrown           = TColor($60A4F4);
  clHTMLSeaGreen             = TColor($578B2E);
  clHTMLSeaShell             = TColor($EEF5FF);
  clHTMLSienna               = TColor($2D52A0);
  clHTMLSilver               = TColor($C0C0C0);
  clHTMLSkyBlue              = TColor($EBCE87);
  clHTMLSlateBlue            = TColor($CD5A6A);
  clHTMLSlateGray            = TColor($908070);
  clHTMLSnow                 = TColor($FAFAFF);
  clHTMLSpringGreen          = TColor($7FFF00);
  clHTMLSteelBlue            = TColor($B48246);
  clHTMLTan                  = TColor($8CB4D2);
  clHTMLTeal                 = TColor($808000);
  clHTMLThistle              = TColor($D8BFD8);
  clHTMLTomato               = TColor($4763FF);
  clHTMLTurquoise            = TColor($D0E040);
  clHTMLViolet               = TColor($EE82EE);
  clHTMLWheat                = TColor($B3DEF5);
  clHTMLWhiteSmoke           = TColor($F5F5F5);
  clHTMLYellow               = TColor($00FFFF);
  clHTMLYellowGreen          = TColor($32CD9A);

var
  htmlcolorsarray : array [0..141] of THtmlColor;

implementation

initialization

htmlcolorsarray[000].Name := 'clHTMLAliceBlue           ';
htmlcolorsarray[001].Name := 'clHTMLAntiqueWhite        ';
htmlcolorsarray[002].Name := 'clHTMLAqua                ';
htmlcolorsarray[003].Name := 'clHTMLAquamarine          ';
htmlcolorsarray[004].Name := 'clHTMLAzure               ';
htmlcolorsarray[005].Name := 'clHTMLBeige               ';
htmlcolorsarray[006].Name := 'clHTMLBisque              ';
htmlcolorsarray[007].Name := 'clHTMLBlack               ';
htmlcolorsarray[008].Name := 'clHTMLBlanchedAlmond      ';
htmlcolorsarray[009].Name := 'clHTMLBlue                ';
htmlcolorsarray[010].Name := 'clHTMLBlueViolet          ';
htmlcolorsarray[011].Name := 'clHTMLBrown               ';
htmlcolorsarray[012].Name := 'clHTMLBurlyWood           ';
htmlcolorsarray[013].Name := 'clHTMLCadetBlue           ';
htmlcolorsarray[014].Name := 'clHTMLChartreuse          ';
htmlcolorsarray[015].Name := 'clHTMLChocolate           ';
htmlcolorsarray[016].Name := 'clHTMLCoral               ';
htmlcolorsarray[017].Name := 'clHTMLCornflowerBlue      ';
htmlcolorsarray[018].Name := 'clHTMLCornsilk            ';
htmlcolorsarray[019].Name := 'clHTMLCrimson             ';
htmlcolorsarray[020].Name := 'clHTMLCyan                ';
htmlcolorsarray[021].Name := 'clHTMLDarkBlue            ';
htmlcolorsarray[022].Name := 'clHTMLDarkCyan            ';
htmlcolorsarray[023].Name := 'clHTMLDarkGoldenRod       ';
htmlcolorsarray[024].Name := 'clHTMLDarkGray            ';
htmlcolorsarray[025].Name := 'clHTMLDarkGreen           ';
htmlcolorsarray[026].Name := 'clHTMLDarkKhaki           ';
htmlcolorsarray[027].Name := 'clHTMLDarkMagenta         ';
htmlcolorsarray[028].Name := 'clHTMLDarkOliveGreen      ';
htmlcolorsarray[029].Name := 'clHTMLDarkOrange          ';
htmlcolorsarray[030].Name := 'clHTMLDarkOrchid          ';
htmlcolorsarray[031].Name := 'clHTMLDarkRed             ';
htmlcolorsarray[032].Name := 'clHTMLDarkSalmon          ';
htmlcolorsarray[033].Name := 'clHTMLDarkSeaGreen        ';
htmlcolorsarray[034].Name := 'clHTMLDarkSlateBlue       ';
htmlcolorsarray[035].Name := 'clHTMLDarkSlateGray       ';
htmlcolorsarray[036].Name := 'clHTMLDarkTurquoise       ';
htmlcolorsarray[037].Name := 'clHTMLDarkViolet          ';
htmlcolorsarray[038].Name := 'clHTMLDeepPink            ';
htmlcolorsarray[039].Name := 'clHTMLDeepSkyBlue         ';
htmlcolorsarray[040].Name := 'clHTMLDimGray             ';
htmlcolorsarray[041].Name := 'clHTMLDodgerBlue          ';
htmlcolorsarray[042].Name := 'clHTMLFireBrick           ';
htmlcolorsarray[043].Name := 'clHTMLFloralWhite         ';
htmlcolorsarray[044].Name := 'clHTMLForestGreen         ';
htmlcolorsarray[045].Name := 'clHTMLFuchsia             ';
htmlcolorsarray[046].Name := 'clHTMLGainsboro           ';
htmlcolorsarray[047].Name := 'clHTMLGhostWhite          ';
htmlcolorsarray[048].Name := 'clHTMLGold                ';
htmlcolorsarray[049].Name := 'clHTMLGoldenRod           ';
htmlcolorsarray[050].Name := 'clHTMLGray                ';
htmlcolorsarray[051].Name := 'clHTMLGreen               ';
htmlcolorsarray[052].Name := 'clHTMLGreenYellow         ';
htmlcolorsarray[053].Name := 'clHTMLGrey                ';
htmlcolorsarray[054].Name := 'clHTMLHoneyDew            ';
htmlcolorsarray[055].Name := 'clHTMLHotPink             ';
htmlcolorsarray[056].Name := 'clHTMLIndianRed           ';
htmlcolorsarray[057].Name := 'clHTMLIndigo              ';
htmlcolorsarray[058].Name := 'clHTMLIvory               ';
htmlcolorsarray[059].Name := 'clHTMLKhaki               ';
htmlcolorsarray[060].Name := 'clHTMLLavender            ';
htmlcolorsarray[061].Name := 'clHTMLLavenderBlush       ';
htmlcolorsarray[062].Name := 'clHTMLLawnGreen           ';
htmlcolorsarray[063].Name := 'clHTMLLemonChiffon        ';
htmlcolorsarray[064].Name := 'clHTMLLightBlue           ';
htmlcolorsarray[065].Name := 'clHTMLLightCoral          ';
htmlcolorsarray[066].Name := 'clHTMLLightCyan           ';
htmlcolorsarray[067].Name := 'clHTMLLightGoldenRodYellow';
htmlcolorsarray[068].Name := 'clHTMLLightGray           ';
htmlcolorsarray[069].Name := 'clHTMLLightGreen          ';
htmlcolorsarray[070].Name := 'clHTMLLightGrey           ';
htmlcolorsarray[071].Name := 'clHTMLLightPink           ';
htmlcolorsarray[072].Name := 'clHTMLLightSalmon         ';
htmlcolorsarray[073].Name := 'clHTMLLightSeaGreen       ';
htmlcolorsarray[074].Name := 'clHTMLLightSkyBlue        ';
htmlcolorsarray[075].Name := 'clHTMLLightSlateGray      ';
htmlcolorsarray[076].Name := 'clHTMLLightSteelBlue      ';
htmlcolorsarray[077].Name := 'clHTMLLightYellow         ';
htmlcolorsarray[078].Name := 'clHTMLLime                ';
htmlcolorsarray[079].Name := 'clHTMLLimeGreen           ';
htmlcolorsarray[080].Name := 'clHTMLLinen               ';
htmlcolorsarray[081].Name := 'clHTMLMagenta             ';
htmlcolorsarray[082].Name := 'clHTMLMaroon              ';
htmlcolorsarray[083].Name := 'clHTMLMediumAquaMarine    ';
htmlcolorsarray[084].Name := 'clHTMLMediumBlue          ';
htmlcolorsarray[085].Name := 'clHTMLMediumOrchid        ';
htmlcolorsarray[086].Name := 'clHTMLMediumPurple        ';
htmlcolorsarray[087].Name := 'clHTMLMediumSeaGreen      ';
htmlcolorsarray[088].Name := 'clHTMLMediumSlateBlue     ';
htmlcolorsarray[089].Name := 'clHTMLMediumSpringGreen   ';
htmlcolorsarray[090].Name := 'clHTMLMediumTurquoise     ';
htmlcolorsarray[091].Name := 'clHTMLMediumVioletRed     ';
htmlcolorsarray[092].Name := 'clHTMLMidnightBlue        ';
htmlcolorsarray[093].Name := 'clHTMLMintCream           ';
htmlcolorsarray[094].Name := 'clHTMLMistyRose           ';
htmlcolorsarray[095].Name := 'clHTMLMoccasin            ';
htmlcolorsarray[096].Name := 'clHTMLNavajoWhite         ';
htmlcolorsarray[097].Name := 'clHTMLNavy                ';
htmlcolorsarray[098].Name := 'clHTMLOldLace             ';
htmlcolorsarray[099].Name := 'clHTMLOlive               ';
htmlcolorsarray[100].Name := 'clHTMLOliveDrab           ';
htmlcolorsarray[101].Name := 'clHTMLOrange              ';
htmlcolorsarray[102].Name := 'clHTMLOrangeRed           ';
htmlcolorsarray[103].Name := 'clHTMLOrchid              ';
htmlcolorsarray[104].Name := 'clHTMLPaleGoldenRod       ';
htmlcolorsarray[105].Name := 'clHTMLPaleGreen           ';
htmlcolorsarray[106].Name := 'clHTMLPaleTurquoise       ';
htmlcolorsarray[107].Name := 'clHTMLPaleVioletRed       ';
htmlcolorsarray[108].Name := 'clHTMLPapayaWhip          ';
htmlcolorsarray[109].Name := 'clHTMLPeachPuff           ';
htmlcolorsarray[110].Name := 'clHTMLPeru                ';
htmlcolorsarray[111].Name := 'clHTMLPink                ';
htmlcolorsarray[112].Name := 'clHTMLPlum                ';
htmlcolorsarray[113].Name := 'clHTMLPowderBlue          ';
htmlcolorsarray[114].Name := 'clHTMLPurple              ';
htmlcolorsarray[115].Name := 'clHTMLRebeccaPurple       ';
htmlcolorsarray[116].Name := 'clHTMLRed                 ';
htmlcolorsarray[117].Name := 'clHTMLRosyBrown           ';
htmlcolorsarray[118].Name := 'clHTMLRoyalBlue           ';
htmlcolorsarray[119].Name := 'clHTMLSaddleBrown         ';
htmlcolorsarray[120].Name := 'clHTMLSalmon              ';
htmlcolorsarray[121].Name := 'clHTMLSandyBrown          ';
htmlcolorsarray[122].Name := 'clHTMLSeaGreen            ';
htmlcolorsarray[123].Name := 'clHTMLSeaShell            ';
htmlcolorsarray[124].Name := 'clHTMLSienna              ';
htmlcolorsarray[125].Name := 'clHTMLSilver              ';
htmlcolorsarray[126].Name := 'clHTMLSkyBlue             ';
htmlcolorsarray[127].Name := 'clHTMLSlateBlue           ';
htmlcolorsarray[128].Name := 'clHTMLSlateGray           ';
htmlcolorsarray[129].Name := 'clHTMLSnow                ';
htmlcolorsarray[130].Name := 'clHTMLSpringGreen         ';
htmlcolorsarray[131].Name := 'clHTMLSteelBlue           ';
htmlcolorsarray[132].Name := 'clHTMLTan                 ';
htmlcolorsarray[133].Name := 'clHTMLTeal                ';
htmlcolorsarray[134].Name := 'clHTMLThistle             ';
htmlcolorsarray[135].Name := 'clHTMLTomato              ';
htmlcolorsarray[136].Name := 'clHTMLTurquoise           ';
htmlcolorsarray[137].Name := 'clHTMLViolet              ';
htmlcolorsarray[138].Name := 'clHTMLWheat               ';
htmlcolorsarray[139].Name := 'clHTMLWhiteSmoke          ';
htmlcolorsarray[140].Name := 'clHTMLYellow              ';
htmlcolorsarray[141].Name := 'clHTMLYellowGreen         ';


htmlcolorsarray[000].Color := clHTMLAliceBlue            ;
htmlcolorsarray[001].Color := clHTMLAntiqueWhite         ;
htmlcolorsarray[002].Color := clHTMLAqua                 ;
htmlcolorsarray[003].Color := clHTMLAquamarine           ;
htmlcolorsarray[004].Color := clHTMLAzure                ;
htmlcolorsarray[005].Color := clHTMLBeige                ;
htmlcolorsarray[006].Color := clHTMLBisque               ;
htmlcolorsarray[007].Color := clHTMLBlack                ;
htmlcolorsarray[008].Color := clHTMLBlanchedAlmond       ;
htmlcolorsarray[009].Color := clHTMLBlue                 ;
htmlcolorsarray[010].Color := clHTMLBlueViolet           ;
htmlcolorsarray[011].Color := clHTMLBrown                ;
htmlcolorsarray[012].Color := clHTMLBurlyWood            ;
htmlcolorsarray[013].Color := clHTMLCadetBlue            ;
htmlcolorsarray[014].Color := clHTMLChartreuse           ;
htmlcolorsarray[015].Color := clHTMLChocolate            ;
htmlcolorsarray[016].Color := clHTMLCoral                ;
htmlcolorsarray[017].Color := clHTMLCornflowerBlue       ;
htmlcolorsarray[018].Color := clHTMLCornsilk             ;
htmlcolorsarray[019].Color := clHTMLCrimson              ;
htmlcolorsarray[020].Color := clHTMLCyan                 ;
htmlcolorsarray[021].Color := clHTMLDarkBlue             ;
htmlcolorsarray[022].Color := clHTMLDarkCyan             ;
htmlcolorsarray[023].Color := clHTMLDarkGoldenRod        ;
htmlcolorsarray[024].Color := clHTMLDarkGray             ;
htmlcolorsarray[025].Color := clHTMLDarkGreen            ;
htmlcolorsarray[026].Color := clHTMLDarkKhaki            ;
htmlcolorsarray[027].Color := clHTMLDarkMagenta          ;
htmlcolorsarray[028].Color := clHTMLDarkOliveGreen       ;
htmlcolorsarray[029].Color := clHTMLDarkOrange           ;
htmlcolorsarray[030].Color := clHTMLDarkOrchid           ;
htmlcolorsarray[031].Color := clHTMLDarkRed              ;
htmlcolorsarray[032].Color := clHTMLDarkSalmon           ;
htmlcolorsarray[033].Color := clHTMLDarkSeaGreen         ;
htmlcolorsarray[034].Color := clHTMLDarkSlateBlue        ;
htmlcolorsarray[035].Color := clHTMLDarkSlateGray        ;
htmlcolorsarray[036].Color := clHTMLDarkTurquoise        ;
htmlcolorsarray[037].Color := clHTMLDarkViolet           ;
htmlcolorsarray[038].Color := clHTMLDeepPink             ;
htmlcolorsarray[039].Color := clHTMLDeepSkyBlue          ;
htmlcolorsarray[040].Color := clHTMLDimGray              ;
htmlcolorsarray[041].Color := clHTMLDodgerBlue           ;
htmlcolorsarray[042].Color := clHTMLFireBrick            ;
htmlcolorsarray[043].Color := clHTMLFloralWhite          ;
htmlcolorsarray[044].Color := clHTMLForestGreen          ;
htmlcolorsarray[045].Color := clHTMLFuchsia              ;
htmlcolorsarray[046].Color := clHTMLGainsboro            ;
htmlcolorsarray[047].Color := clHTMLGhostWhite           ;
htmlcolorsarray[048].Color := clHTMLGold                 ;
htmlcolorsarray[049].Color := clHTMLGoldenRod            ;
htmlcolorsarray[050].Color := clHTMLGray                 ;
htmlcolorsarray[051].Color := clHTMLGreen                ;
htmlcolorsarray[052].Color := clHTMLGreenYellow          ;
htmlcolorsarray[053].Color := clHTMLGrey                 ;
htmlcolorsarray[054].Color := clHTMLHoneyDew             ;
htmlcolorsarray[055].Color := clHTMLHotPink              ;
htmlcolorsarray[056].Color := clHTMLIndianRed            ;
htmlcolorsarray[057].Color := clHTMLIndigo               ;
htmlcolorsarray[058].Color := clHTMLIvory                ;
htmlcolorsarray[059].Color := clHTMLKhaki                ;
htmlcolorsarray[060].Color := clHTMLLavender             ;
htmlcolorsarray[061].Color := clHTMLLavenderBlush        ;
htmlcolorsarray[062].Color := clHTMLLawnGreen            ;
htmlcolorsarray[063].Color := clHTMLLemonChiffon         ;
htmlcolorsarray[064].Color := clHTMLLightBlue            ;
htmlcolorsarray[065].Color := clHTMLLightCoral           ;
htmlcolorsarray[066].Color := clHTMLLightCyan            ;
htmlcolorsarray[067].Color := clHTMLLightGoldenRodYellow ;
htmlcolorsarray[068].Color := clHTMLLightGray            ;
htmlcolorsarray[069].Color := clHTMLLightGreen           ;
htmlcolorsarray[070].Color := clHTMLLightGrey            ;
htmlcolorsarray[071].Color := clHTMLLightPink            ;
htmlcolorsarray[072].Color := clHTMLLightSalmon          ;
htmlcolorsarray[073].Color := clHTMLLightSeaGreen        ;
htmlcolorsarray[074].Color := clHTMLLightSkyBlue         ;
htmlcolorsarray[075].Color := clHTMLLightSlateGray       ;
htmlcolorsarray[076].Color := clHTMLLightSteelBlue       ;
htmlcolorsarray[077].Color := clHTMLLightYellow          ;
htmlcolorsarray[078].Color := clHTMLLime                 ;
htmlcolorsarray[079].Color := clHTMLLimeGreen            ;
htmlcolorsarray[080].Color := clHTMLLinen                ;
htmlcolorsarray[081].Color := clHTMLMagenta              ;
htmlcolorsarray[082].Color := clHTMLMaroon               ;
htmlcolorsarray[083].Color := clHTMLMediumAquaMarine     ;
htmlcolorsarray[084].Color := clHTMLMediumBlue           ;
htmlcolorsarray[085].Color := clHTMLMediumOrchid         ;
htmlcolorsarray[086].Color := clHTMLMediumPurple         ;
htmlcolorsarray[087].Color := clHTMLMediumSeaGreen       ;
htmlcolorsarray[088].Color := clHTMLMediumSlateBlue      ;
htmlcolorsarray[089].Color := clHTMLMediumSpringGreen    ;
htmlcolorsarray[090].Color := clHTMLMediumTurquoise      ;
htmlcolorsarray[091].Color := clHTMLMediumVioletRed      ;
htmlcolorsarray[092].Color := clHTMLMidnightBlue         ;
htmlcolorsarray[093].Color := clHTMLMintCream            ;
htmlcolorsarray[094].Color := clHTMLMistyRose            ;
htmlcolorsarray[095].Color := clHTMLMoccasin             ;
htmlcolorsarray[096].Color := clHTMLNavajoWhite          ;
htmlcolorsarray[097].Color := clHTMLNavy                 ;
htmlcolorsarray[098].Color := clHTMLOldLace              ;
htmlcolorsarray[099].Color := clHTMLOlive                ;
htmlcolorsarray[100].Color := clHTMLOliveDrab            ;
htmlcolorsarray[101].Color := clHTMLOrange               ;
htmlcolorsarray[102].Color := clHTMLOrangeRed            ;
htmlcolorsarray[103].Color := clHTMLOrchid               ;
htmlcolorsarray[104].Color := clHTMLPaleGoldenRod        ;
htmlcolorsarray[105].Color := clHTMLPaleGreen            ;
htmlcolorsarray[106].Color := clHTMLPaleTurquoise        ;
htmlcolorsarray[107].Color := clHTMLPaleVioletRed        ;
htmlcolorsarray[108].Color := clHTMLPapayaWhip           ;
htmlcolorsarray[109].Color := clHTMLPeachPuff            ;
htmlcolorsarray[110].Color := clHTMLPeru                 ;
htmlcolorsarray[111].Color := clHTMLPink                 ;
htmlcolorsarray[112].Color := clHTMLPlum                 ;
htmlcolorsarray[113].Color := clHTMLPowderBlue           ;
htmlcolorsarray[114].Color := clHTMLPurple               ;
htmlcolorsarray[115].Color := clHTMLRebeccaPurple        ;
htmlcolorsarray[116].Color := clHTMLRed                  ;
htmlcolorsarray[117].Color := clHTMLRosyBrown            ;
htmlcolorsarray[118].Color := clHTMLRoyalBlue            ;
htmlcolorsarray[119].Color := clHTMLSaddleBrown          ;
htmlcolorsarray[120].Color := clHTMLSalmon               ;
htmlcolorsarray[121].Color := clHTMLSandyBrown           ;
htmlcolorsarray[122].Color := clHTMLSeaGreen             ;
htmlcolorsarray[123].Color := clHTMLSeaShell             ;
htmlcolorsarray[124].Color := clHTMLSienna               ;
htmlcolorsarray[125].Color := clHTMLSilver               ;
htmlcolorsarray[126].Color := clHTMLSkyBlue              ;
htmlcolorsarray[127].Color := clHTMLSlateBlue            ;
htmlcolorsarray[128].Color := clHTMLSlateGray            ;
htmlcolorsarray[129].Color := clHTMLSnow                 ;
htmlcolorsarray[130].Color := clHTMLSpringGreen          ;
htmlcolorsarray[131].Color := clHTMLSteelBlue            ;
htmlcolorsarray[132].Color := clHTMLTan                  ;
htmlcolorsarray[133].Color := clHTMLTeal                 ;
htmlcolorsarray[134].Color := clHTMLThistle              ;
htmlcolorsarray[135].Color := clHTMLTomato               ;
htmlcolorsarray[136].Color := clHTMLTurquoise            ;
htmlcolorsarray[137].Color := clHTMLViolet               ;
htmlcolorsarray[138].Color := clHTMLWheat                ;
htmlcolorsarray[139].Color := clHTMLWhiteSmoke           ;
htmlcolorsarray[140].Color := clHTMLYellow               ;
htmlcolorsarray[141].Color := clHTMLYellowGreen          ;


end.
