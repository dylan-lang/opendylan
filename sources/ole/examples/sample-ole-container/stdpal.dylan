module: sample-OLE-container
description: Standard App Palette useful for OLE applications.  v 1.01
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant palSVGA :: <array> = #[

	// R     G     B
	#[#x00, #x00, #x00], // 0 Sys Black  gray 0
	#[#x80, #x00, #x00], // 1 Sys Dk Red
	#[#x00, #x80, #x00], // 2 Sys Dk Green
	#[#x80, #x80, #x00], // 3 Sys Dk Yellow
	#[#x00, #x00, #x80], // 4 Sys Dk Blue
	#[#x80, #x00, #x80], // 5 Sys Dk Violet
	#[#x00, #x80, #x80], // 6 Sys Dk Cyan
	#[#xc0, #xc0, #xc0], // 7 Sys Lt Grey    gray 192
	#[#xc0, #xdc, #xc0], // 8 Sys 8
	#[#xa6, #xca, #xf0], // 9 Sys 9 (the first 10 are fixed by Windows)

	#[#x80, #x00, #x00], // 10 Sys Dk Red repeat
	#[#x00, #x80, #x00], // 11 Sys Dk Green repeat
	#[#x80, #x80, #x00], // 12 Sys Dk Yellow repeat
	#[#x00, #x00, #x80], // 13 Sys Dk Blue repeat
	#[#x80, #x00, #x80], // 14 Sys Dk Violet repeat
	#[#x00, #x80, #x80], // 15 Sys Dk Cyan repeat
	#[#x80, #x80, #x80], // 16 Sys Dk Grey repeat    gray 128
	#[#x80, #x80, #xff], // 17 Excel Chart Fill 1
	#[#x80, #x20, #x60], // 18 Excel Chart Fill 2
	#[#xff, #xff, #xc0], // 19 Excel Chart Fill 3
	#[#xa0, #xe0, #xe0], // 20 Excel Chart Fill 4
	#[#x60, #x00, #x80], // 21 Excel Chart Fill 4
	#[#xff, #x80, #x80], // 22 Excel Chart Fill 6
	#[#x00, #x80, #xc0], // 23 Excel Chart Fill 7
	#[#xc0, #xc0, #xff], // 24 Excel Chart Fill 8
	#[#x00, #xcf, #xff], // 25 Excel clrt entry
	#[#x69, #xff, #xff], // 26 Excel clrt entry
	#[#xe0, #xff, #xe0], // 27 Excel clrt entry
	#[#xdd, #x9c, #xb3], // 28 Excel clrt entry
	#[#xb3, #x8f, #xee], // 29 Excel clrt entry
	#[#x2a, #x6f, #xf9], // 30 Excel clrt entry
	#[#x3f, #xb8, #xcd], // 31 Excel clrt entry
	#[#x48, #x84, #x36], // 32 Excel clrt entry
	#[#x95, #x8c, #x41], // 33 Excel clrt entry
	#[#x8e, #x5e, #x42], // 34 Excel clrt entry
	#[#xa0, #x62, #x7a], // 35 Excel clrt entry
	#[#x62, #x4f, #xac], // 36 Excel clrt entry
	#[#x1d, #x2f, #xbe], // 37 Excel clrt entry
	#[#x28, #x66, #x76], // 38 Excel clrt entry
	#[#x00, #x45, #x00], // 39 Excel clrt entry
	#[#x45, #x3e, #x01], // 40 Excel clrt entry
	#[#x6a, #x28, #x13], // 41 Excel clrt entry
	#[#x85, #x39, #x6a], // 42 Excel clrt entry
	#[#x4a, #x32, #x85], // 43 Excel clrt entry
	#[#x04, #x04, #x04], // 44   gray 4
	#[#x08, #x08, #x08], // 45   gray 8
	#[#x0c, #x0c, #x0c], // 46   gray 12
	#[#x11, #x11, #x11], // 47   gray 17
	#[#x16, #x16, #x16], // 48   gray 22
	#[#x1c, #x1c, #x1c], // 49   gray 28
	#[#x22, #x22, #x22], // 50   gray 34
	#[#x29, #x29, #x29], // 51   gray 41
	#[#x30, #x30, #x30], // 52   gray 48
	#[#x5f, #x5f, #x5f], // 53 swapped so inversions look good   gray 95
	#[#x55, #x55, #x55], // 54 swapped so inversions look good   gray 85
	#[#x4d, #x4d, #x4d], // 55 swapped so inversions look good   gray 77
	#[#x42, #x42, #x42], // 56 swapped so inversions look good   gray 66
	#[#x39, #x39, #x39], // 57 swapped so inversions look good   gray 57
	#[#x00, #x07, #x00], // 58
	#[#x0d, #x00, #x00], // 59
	#[#xb7, #x99, #x81], // 60
	#[#x84, #x99, #xb4], // 61
	#[#xbd, #xbd, #x90], // 62
	#[#x7f, #x7f, #x60], // 63
	#[#x60, #x60, #x7f], // 64
	#[#x00, #x0e, #x00], // 65
	#[#x1b, #x00, #x00], // 66
	#[#x28, #x00, #x00], // 67
	#[#x08, #x09, #x2b], // 68
	#[#x00, #x1d, #x00], // 69
	#[#x39, #x00, #x00], // 70
	#[#x00, #x00, #x9b], // 71
	#[#x00, #x25, #x00], // 72
	#[#x49, #x00, #x00], // 73
	#[#x11, #x11, #x3b], // 74
	#[#x00, #x2f, #x00], // 75
	#[#x5d, #x00, #x00], // 76
	#[#x17, #x17, #x45], // 77
	#[#x00, #x3a, #x00], // 78
	#[#x49, #x11, #x11], // 79
	#[#x1c, #x1c, #x53], // 80
	#[#x00, #x16, #xff], // 81
	#[#x2b, #x00, #xff], // 82
	#[#x21, #x21, #x6c], // 83
	#[#x59, #x14, #x14], // 84
	#[#x00, #x51, #x00], // 85
	#[#x47, #x1a, #x6a], // 86
	#[#x19, #x32, #x67], // 87
	#[#x00, #x61, #x00], // 88
	#[#x00, #x31, #xff], // 89
	#[#x61, #x00, #xff], // 90
	#[#x53, #x20, #x7b], // 91
	#[#x16, #x43, #x67], // 92
	#[#x2e, #x2e, #xe2], // 93
	#[#x26, #x59, #x16], // 94
	#[#x51, #x46, #x04], // 95
	#[#x68, #x2e, #x49], // 96
	#[#x07, #x52, #x8f], // 97
	#[#x6a, #x18, #xb8], // 98
	#[#x90, #x23, #x15], // 99
	#[#x00, #x53, #xff], // 100
	#[#xa3, #x00, #xff], // 101
	#[#x6a, #x4a, #x12], // 102
	#[#x75, #x33, #x6c], // 103
	#[#x4a, #x41, #x9a], // 104
	#[#x37, #x65, #x0b], // 105
	#[#xa4, #x2c, #x15], // 106
	#[#x83, #x1f, #xb1], // 107
	#[#x4e, #x2c, #xff], // 108
	#[#x20, #x51, #xb6], // 109
	#[#x08, #x64, #x92], // 110
	#[#x6f, #x56, #x0b], // 111
	#[#x59, #x43, #xad], // 112
	#[#x36, #x72, #x12], // 113
	#[#xb0, #x33, #x17], // 114
	#[#x00, #xa1, #x00], // 115
	#[#x77, #x5f, #x1f], // 116
	#[#x89, #x47, #x71], // 117
	#[#xb0, #x43, #x1c], // 118
	#[#xb7, #x2d, #x7d], // 119
	#[#x00, #x86, #x95], // 120
	#[#x7a, #x6e, #x23], // 121
	#[#x26, #x9f, #x00], // 122
	#[#x73, #xa9, #x01], // 123
	#[#x00, #x00, #x00], // 124 free 0   gray 0
	#[#x00, #x00, #x00], // 125 free 2   gray 0
	#[#x00, #x00, #x00], // 126 free 4   gray 0
	#[#x00, #x00, #x00], // 127 free 6   gray 0
	#[#x00, #x00, #x00], // 128 free 7   gray 0
	#[#x00, #x00, #x00], // 129 free 5   gray 0
	#[#x00, #x00, #x00], // 130 free 3   gray 0
	#[#x00, #x00, #x00], // 131 free 1   gray 0
	#[#x00, #xca, #x00], // 132
	#[#xac, #x5b, #x01], // 133
	#[#x20, #x1d, #xc2], // 134
	#[#x94, #x52, #x70], // 135
	#[#x24, #xaa, #x4c], // 136
	#[#x0a, #x94, #x89], // 137
	#[#x36, #x6e, #x7b], // 138
	#[#x44, #x75, #x90], // 139
	#[#xff, #x00, #xa8], // 140
	#[#x00, #x71, #xff], // 141
	#[#xdf, #x00, #xff], // 142
	#[#x56, #x91, #x4a], // 143
	#[#x34, #x48, #xf8], // 144
	#[#xcc, #x32, #x82], // 145
	#[#xe4, #x41, #x70], // 146
	#[#x68, #xca, #x01], // 147
	#[#x36, #xbc, #x42], // 148
	#[#x00, #x9a, #xff], // 149
	#[#x96, #x22, #xb7], // 150
	#[#x85, #x7d, #x33], // 151
	#[#x25, #xb7, #x8c], // 152
	#[#x36, #x5a, #xed], // 153
	#[#x5c, #xff, #x00], // 154
	#[#xff, #x48, #x00], // 155
	#[#x22, #x9b, #xa2], // 156
	#[#x42, #xcf, #x4d], // 157
	#[#xc2, #x58, #x52], // 158
	#[#x20, #xd3, #x95], // 159
	#[#xa5, #x24, #xe0], // 160
	#[#x73, #x56, #xb5], // 161
	#[#xa9, #xa9, #x00], // 162
	#[#xd0, #x6f, #x3c], // 163
	#[#x67, #x9f, #x58], // 164
	#[#x89, #xcf, #x0b], // 165
	#[#xff, #xac, #x00], // 166
	#[#xa7, #x2e, #xfe], // 167
	#[#xe2, #x59, #x7f], // 168
	#[#x4c, #xdc, #x67], // 169
	#[#xff, #x18, #xff], // 170
	#[#x3a, #x7d, #xff], // 171
	#[#xb1, #xd0, #x18], // 172
	#[#xc7, #xff, #x00], // 173
	#[#xff, #xe2, #x00], // 174
	#[#xdf, #x9a, #x3d], // 175
	#[#x56, #x81, #x9f], // 176
	#[#xc6, #x43, #xba], // 177
	#[#xaf, #x71, #x8b], // 178
	#[#x38, #xa2, #xc9], // 179
	#[#xd1, #x53, #xce], // 180
	#[#xff, #x9a, #x65], // 181
	#[#x46, #xca, #xdb], // 182
	#[#xff, #x4d, #xff], // 183
	#[#xc8, #xe9, #x6a], // 184
	#[#x4c, #xde, #xe0], // 185
	#[#xff, #x98, #xff], // 186
	#[#xdf, #xc0, #x82], // 187
	#[#xe9, #xec, #xa5], // 188
	#[#xf5, #xf6, #xcd], // 189
	#[#xff, #xd0, #xff], // 190
	#[#xb1, #xac, #x5a], // 191
	#[#x63, #x91, #xae], // 192
	#[#x22, #x4c, #x65], // 193
	#[#x8d, #x4e, #x3f], // 194
	#[#x50, #x70, #x70], // 195
	#[#xd0, #xff, #xff], // 196
	#[#xff, #xe7, #xff], // 197
	#[#x69, #x69, #x69], // 198  gray 105
	#[#x77, #x77, #x77], // 199  gray 119
	#[#x86, #x86, #x86], // 200  gray 134
	#[#x96, #x96, #x96], // 201  gray 150
	#[#x9d, #x9d, #x9d], // 202  gray 157
	#[#xa4, #xa4, #xa4], // 203  gray 164
	#[#xb2, #xb2, #xb2], // 204  gray 178
	#[#xcb, #xcb, #xcb], // 205  gray 203
	#[#xd7, #xd7, #xd7], // 206  gray 215
	#[#xdd, #xdd, #xdd], // 207  gray 221
	#[#xe3, #xe3, #xe3], // 208  gray 227
	#[#xea, #xea, #xea], // 209  gray 234
	#[#xf1, #xf1, #xf1], // 210  gray 241
	#[#xf8, #xf8, #xf8], // 211  gray 248
	#[#xb2, #xc1, #x66], // 212
	#[#x80, #xbf, #x78], // 213
	#[#xc6, #xf0, #xf0], // 214
	#[#xb2, #xa4, #xff], // 215
	#[#xff, #xb3, #xff], // 216
	#[#xd1, #x8e, #xa3], // 217
	#[#xc3, #xdc, #x37], // 218
	#[#xa0, #x9e, #x54], // 219
	#[#x76, #xae, #x70], // 220
	#[#x78, #x9e, #xc1], // 221
	#[#x83, #x64, #xbf], // 222
	#[#xa4, #x83, #xd3], // 223
	#[#xd1, #x3f, #x32], // 224
	#[#xff, #x7d, #x00], // 225
	#[#x44, #x78, #x23], // 226
	#[#x24, #x5f, #x60], // 227
	#[#x0e, #x0e, #x2c], // 228
	#[#xbe, #x00, #x00], // 229
	#[#xff, #x1f, #x00], // 230
	#[#x31, #x39, #x00], // 231
	#[#xd9, #x85, #x3e], // 232
	#[#x02, #x77, #x85], // 233
	#[#xb0, #xd8, #x81], // 234
	#[#x56, #x21, #x1d], // 235
	#[#x00, #x00, #x30], // 236
	#[#x88, #xc8, #xb3], // 237
	#[#xa0, #x79, #x00], // 238
	#[#xc0, #xc0, #xc0], // 239 Sys Dk Grey repeat inversion gray 192
	#[#xea, #x70, #x81], // 240
	#[#x51, #xf1, #x69], // 241
	#[#xff, #xff, #x80], // 242
	#[#x91, #x74, #xcd], // 243
	#[#xff, #x7c, #xff], // 244
	#[#xa2, #xff, #xff], // 245

	#[#xff, #xfb, #xf0], // 246 Sys Reserved
	#[#xa0, #xa0, #xa4], // 247 Sys Reserved
	#[#x80, #x80, #x80], // 248 Sys Lt Gray  gray 128
	#[#xff, #x00, #x00], // 249 Sys Red
	#[#x00, #xff, #x00], // 250 Sys Green
	#[#xff, #xff, #x00], // 251 Sys Yellow
	#[#x00, #x00, #xff], // 252 Sys Blue
	#[#xff, #x00, #xff], // 253 Sys Violet
	#[#x00, #xff, #xff], // 254 Sys Cyan
	#[#xff, #xff, #xff] // 255 Sys White gray 255
	];
