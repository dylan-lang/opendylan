Module:    OLE-Container
Synopsis:  Support for color palettes
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define constant cpeAppPal = 256;  // number of colors in our apps palette

define C-struct <LOGPAL>
  inline-only slot wVersion   :: <WORD>;
  inline-only slot cpe :: <WORD>;
  inline-only array slot rgpe-array :: <PALETTEENTRY>, length: cpeAppPal,
	address-getter: rgpe, setter: #f;
  pointer-type-name: <PLOGPAL>;
end <LOGPAL>;

ignorable(wVersion, cpe, rgpe-array); // to suppress warnings about unused

//**********************************************************************
//
// create-standard-palette
//
// Purpose:
//
//      Creates a standard Apps palette.
//
// Parameters:
//
//      None
//
// Return Value:
//
//      None
//
//********************************************************************

define method create-standard-palette() => value :: <HPALETTE>;
   
  let hpal :: <HPALETTE> = null-pointer(<HPALETTE>);
  let hdc :: <HDC> = GetDC($NULL-HWND);
  if ( ( ~ null-handle?(hdc) )
	& ~ zero?(logand( GetDeviceCaps(hdc, $RASTERCAPS), $RC-PALETTE)) )
      
    let cpeSysPal :: <fixnum> = GetDeviceCaps( hdc, $SIZEPALETTE);
    let cpeReserved :: <fixnum> = GetDeviceCaps( hdc, $NUMRESERVED);
    if ( cpeSysPal > cpeReserved )
	 
      let logpal :: <PLOGPAL> = make(<PLOGPAL>);
      let cpeReserved2 :: <fixnum> = truncate/(cpeReserved,2);

      // Get the system palette entries at the beginning and end.
      GetSystemPaletteEntries( hdc, 0, cpeReserved2, logpal.rgpe);
      GetSystemPaletteEntries( hdc, cpeSysPal - cpeReserved2, cpeReserved2,
		      pointer-value-address(logpal.rgpe,
					    index: cpeAppPal - cpeReserved2));

      logpal.cpe := cpeAppPal;
      logpal.wVersion := #x300;

      for ( i :: <unsigned-fixnum> from cpeReserved2
	     below cpeAppPal - cpeReserved2,
	    j :: <unsigned-fixnum> from 10 )
	let RGB-color :: <integer> = $standard-palette-colors[j];
	let rgpei :: <LPPALETTEENTRY> =
	  pointer-value-address(logpal.rgpe, index: i);
	rgpei.peFlags-value := $PC-NOCOLLAPSE;
	rgpei.peRed-value   := ash(RGB-color, -16);
	rgpei.peGreen-value := logand(ash(RGB-color, -8), #xFF);
	rgpei.peBlue-value  := logand(RGB-color, #xFF);
      end for;

      hpal := CreatePalette( pointer-cast(<LPLOGPALETTE>, logpal));
      // check for error???
      destroy(logpal);
    end if;
  end if;
  ReleaseDC($NULL-HWND, hdc);
  hpal
end method create-standard-palette;


// This is just a helper routine 
define method select-palette(hWnd :: <HWND>, hPal :: <HPALETTE>,
			     fBackground :: <boolean>)
	=> value :: <fixnum>;
  if ( null-handle?(hPal) )
    0
  else
    let hdc :: <HDC> = GetDC(hWnd);
    let hOldPal :: <HPALETTE> = SelectPalette(hdc, hPal, fBackground);
    let iPalChg :: <unsigned-fixnum> = RealizePalette(hdc);
    SelectPalette(hdc, hOldPal, #t /* fBackground */ );
    ReleaseDC(hWnd, hdc);

    if ( iPalChg >  0 )
      InvalidateRect(hWnd, $NULL-RECT, #t);
    end if;
    1
  end if
end method select-palette;

// Standard App Palette useful for OLE applications.
define constant $standard-palette-colors :: <simple-vector> = #[
	//R G B
	#x000000, // 0 Sys Black  gray 0
	#x800000, // 1 Sys Dk Red
	#x008000, // 2 Sys Dk Green
	#x808000, // 3 Sys Dk Yellow
	#x000080, // 4 Sys Dk Blue
	#x800080, // 5 Sys Dk Violet
	#x008080, // 6 Sys Dk Cyan
	#xC0C0C0, // 7 Sys Lt Grey    gray 192
	#xC0DCC0, // 8 Sys 8
	#xA6CAF0, // 9 Sys 9 (the first 10 are fixed by Windows)

	#x800000, // 10 Sys Dk Red repeat
	#x008000, // 11 Sys Dk Green repeat
	#x808000, // 12 Sys Dk Yellow repeat
	#x000080, // 13 Sys Dk Blue repeat
	#x800080, // 14 Sys Dk Violet repeat
	#x008080, // 15 Sys Dk Cyan repeat
	#x808080, // 16 Sys Dk Grey repeat    gray 128
	#x8080FF, // 17 Excel Chart Fill 1
	#x802060, // 18 Excel Chart Fill 2
	#xFFFFC0, // 19 Excel Chart Fill 3
	#xA0E0E0, // 20 Excel Chart Fill 4
	#x600080, // 21 Excel Chart Fill 4
	#xFF8080, // 22 Excel Chart Fill 6
	#x0080C0, // 23 Excel Chart Fill 7
	#xC0C0FF, // 24 Excel Chart Fill 8
	#x00CFFF, // 25 Excel clrt entry
	#x69FFFF, // 26 Excel clrt entry
	#xE0FFE0, // 27 Excel clrt entry
	#xDD9CB3, // 28 Excel clrt entry
	#xB38FEE, // 29 Excel clrt entry
	#x2A6FF9, // 30 Excel clrt entry
	#x3FB8CD, // 31 Excel clrt entry
	#x488436, // 32 Excel clrt entry
	#x958C41, // 33 Excel clrt entry
	#x8E5E42, // 34 Excel clrt entry
	#xA0627A, // 35 Excel clrt entry
	#x624FAC, // 36 Excel clrt entry
	#x1D2FBE, // 37 Excel clrt entry
	#x286676, // 38 Excel clrt entry
	#x004500, // 39 Excel clrt entry
	#x453E01, // 40 Excel clrt entry
	#x6A2813, // 41 Excel clrt entry
	#x85396A, // 42 Excel clrt entry
	#x4A3285, // 43 Excel clrt entry
	#x040404, // 44   gray 4
	#x080808, // 45   gray 8
	#x0C0C0C, // 46   gray 12
	#x111111, // 47   gray 17
	#x161616, // 48   gray 22
	#x1C1C1C, // 49   gray 28
	#x222222, // 50   gray 34
	#x292929, // 51   gray 41
	#x303030, // 52   gray 48
	#x5F5F5F, // 53 swapped so inversions look good   gray 95
	#x555555, // 54 swapped so inversions look good   gray 85
	#x4D4D4D, // 55 swapped so inversions look good   gray 77
	#x424242, // 56 swapped so inversions look good   gray 66
	#x393939, // 57 swapped so inversions look good   gray 57
	#x000700, // 58
	#x0D0000, // 59
	#xB79981, // 60
	#x8499B4, // 61
	#xBDBD90, // 62
	#x7F7F60, // 63
	#x60607F, // 64
	#x000E00, // 65
	#x1B0000, // 66
	#x280000, // 67
	#x08092B, // 68
	#x001D00, // 69
	#x390000, // 70
	#x00009B, // 71
	#x002500, // 72
	#x490000, // 73
	#x11113B, // 74
	#x002F00, // 75
	#x5D0000, // 76
	#x171745, // 77
	#x003A00, // 78
	#x491111, // 79
	#x1C1C53, // 80
	#x0016FF, // 81
	#x2B00FF, // 82
	#x21216C, // 83
	#x591414, // 84
	#x005100, // 85
	#x471A6A, // 86
	#x193267, // 87
	#x006100, // 88
	#x0031FF, // 89
	#x6100FF, // 90
	#x53207B, // 91
	#x164367, // 92
	#x2E2EE2, // 93
	#x265916, // 94
	#x514604, // 95
	#x682E49, // 96
	#x07528F, // 97
	#x6A18B8, // 98
	#x902315, // 99
	#x0053FF, // 100
	#xA300FF, // 101
	#x6A4A12, // 102
	#x75336C, // 103
	#x4A419A, // 104
	#x37650B, // 105
	#xA42C15, // 106
	#x831FB1, // 107
	#x4E2CFF, // 108
	#x2051B6, // 109
	#x086492, // 110
	#x6F560B, // 111
	#x5943AD, // 112
	#x367212, // 113
	#xB03317, // 114
	#x00A100, // 115
	#x775F1F, // 116
	#x894771, // 117
	#xB0431C, // 118
	#xB72D7D, // 119
	#x008695, // 120
	#x7A6E23, // 121
	#x269F00, // 122
	#x73A901, // 123
	#x000000, // 124 free 0   gray 0
	#x000000, // 125 free 2   gray 0
	#x000000, // 126 free 4   gray 0
	#x000000, // 127 free 6   gray 0
	#x000000, // 128 free 7   gray 0
	#x000000, // 129 free 5   gray 0
	#x000000, // 130 free 3   gray 0
	#x000000, // 131 free 1   gray 0
	#x00CA00, // 132
	#xAC5B01, // 133
	#x201DC2, // 134
	#x945270, // 135
	#x24AA4C, // 136
	#x0A9489, // 137
	#x366E7B, // 138
	#x447590, // 139
	#xFF00A8, // 140
	#x0071FF, // 141
	#xDF00FF, // 142
	#x56914A, // 143
	#x3448F8, // 144
	#xCC3282, // 145
	#xE44170, // 146
	#x68CA01, // 147
	#x36BC42, // 148
	#x009AFF, // 149
	#x9622B7, // 150
	#x857D33, // 151
	#x25B78C, // 152
	#x365AED, // 153
	#x5CFF00, // 154
	#xFF4800, // 155
	#x229BA2, // 156
	#x42CF4D, // 157
	#xC25852, // 158
	#x20D395, // 159
	#xA524E0, // 160
	#x7356B5, // 161
	#xA9A900, // 162
	#xD06F3C, // 163
	#x679F58, // 164
	#x89CF0B, // 165
	#xFFAC00, // 166
	#xA72EFE, // 167
	#xE2597F, // 168
	#x4CDC67, // 169
	#xFF18FF, // 170
	#x3A7DFF, // 171
	#xB1D018, // 172
	#xC7FF00, // 173
	#xFFE200, // 174
	#xDF9A3D, // 175
	#x56819F, // 176
	#xC643BA, // 177
	#xAF718B, // 178
	#x38A2C9, // 179
	#xD153CE, // 180
	#xFF9A65, // 181
	#x46CADB, // 182
	#xFF4DFF, // 183
	#xC8E96A, // 184
	#x4CDEE0, // 185
	#xFF98FF, // 186
	#xDFC082, // 187
	#xE9ECA5, // 188
	#xF5F6CD, // 189
	#xFFD0FF, // 190
	#xB1AC5A, // 191
	#x6391AE, // 192
	#x224C65, // 193
	#x8D4E3F, // 194
	#x507070, // 195
	#xD0FFFF, // 196
	#xFFE7FF, // 197
	#x696969, // 198  gray 105
	#x777777, // 199  gray 119
	#x868686, // 200  gray 134
	#x969696, // 201  gray 150
	#x9D9D9D, // 202  gray 157
	#xA4A4A4, // 203  gray 164
	#xB2B2B2, // 204  gray 178
	#xCBCBCB, // 205  gray 203
	#xD7D7D7, // 206  gray 215
	#xDDDDDD, // 207  gray 221
	#xE3E3E3, // 208  gray 227
	#xEAEAEA, // 209  gray 234
	#xF1F1F1, // 210  gray 241
	#xF8F8F8, // 211  gray 248
	#xB2C166, // 212
	#x80BF78, // 213
	#xC6F0F0, // 214
	#xB2A4FF, // 215
	#xFFB3FF, // 216
	#xD18EA3, // 217
	#xC3DC37, // 218
	#xA09E54, // 219
	#x76AE70, // 220
	#x789EC1, // 221
	#x8364BF, // 222
	#xA483D3, // 223
	#xD13F32, // 224
	#xFF7D00, // 225
	#x447823, // 226
	#x245F60, // 227
	#x0E0E2C, // 228
	#xBE0000, // 229
	#xFF1F00, // 230
	#x313900, // 231
	#xD9853E, // 232
	#x027785, // 233
	#xB0D881, // 234
	#x56211D, // 235
	#x000030, // 236
	#x88C8B3, // 237
	#xA07900, // 238
	#xC0C0C0, // 239 Sys Dk Grey repeat inversion gray 192
	#xEA7081, // 240
	#x51F169, // 241
	#xFFFF80, // 242
	#x9174CD, // 243
	#xFF7CFF, // 244
	#xA2FFFF, // 245

	#xFFFBF0, // 246 Sys Reserved
	#xA0A0A4, // 247 Sys Reserved
	#x808080, // 248 Sys Lt Gray  gray 128
	#xFF0000, // 249 Sys Red
	#x00FF00, // 250 Sys Green
	#xFFFF00, // 251 Sys Yellow
	#x0000FF, // 252 Sys Blue
	#xFF00FF, // 253 Sys Violet
	#x00FFFF, // 254 Sys Cyan
	#xFFFFFF  // 255 Sys White gray 255
	];
