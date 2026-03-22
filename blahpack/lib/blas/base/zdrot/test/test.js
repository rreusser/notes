/**
* @license Apache-2.0
*
* Copyright (c) 2025 The Stdlib Authors.
*
* Licensed under the Apache License, Version 2.0 (the "License");
* you may not use this file except in compliance with the License.
* You may obtain a copy of the License at
*
*    http://www.apache.org/licenses/LICENSE-2.0
*
* Unless required by applicable law or agreed to in writing, software
* distributed under the License is distributed on an "AS IS" BASIS,
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
* See the License for the specific language governing permissions and
* limitations under the License.
*/

'use strict';

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var zdrot = require( './../lib' );
var base = require( './../lib/base.js' );

// HELPERS //

function assertClose( actual, expected, msg ) {
	var relErr = Math.abs( actual - expected ) / Math.max( Math.abs( expected ), 1.0 );
	assert.ok( relErr <= 1e-14, msg + ': expected ' + expected + ', got ' + actual );
}

function assertArrayClose( actual, expected, msg ) {
	var i;
	assert.strictEqual( actual.length, expected.length, msg + ': length mismatch' );
	for ( i = 0; i < expected.length; i++ ) {
		assertClose( actual[ i ], expected[ i ], msg + '[' + i + ']' );
	}
}

// TESTS //

test( 'zdrot: main export is a function', function t() {
	assert.strictEqual( typeof zdrot, 'function' );
});

test( 'zdrot: attached to the main export is an `ndarray` method', function t() {
	assert.strictEqual( typeof zdrot.ndarray, 'function' );
});

test( 'zdrot: N=0 is a no-op', function t() {
	// When N <= 0, arrays should remain unchanged
	var zx = new Float64Array( [ 1.0, 2.0, 3.0, 4.0 ] );
	var zy = new Float64Array( [ 5.0, 6.0, 7.0, 8.0 ] );
	var result = base( 0, zx, 1, 0, zy, 1, 0, 0.6, 0.8 );
	assert.strictEqual( result, zx );
	assertArrayClose( Array.from( zx ), [ 1.0, 2.0, 3.0, 4.0 ], 'zx unchanged' );
	assertArrayClose( Array.from( zy ), [ 5.0, 6.0, 7.0, 8.0 ], 'zy unchanged' );
});

test( 'zdrot: negative N is a no-op', function t() {
	var zx = new Float64Array( [ 1.0, 2.0 ] );
	var zy = new Float64Array( [ 3.0, 4.0 ] );
	var result = base( -1, zx, 1, 0, zy, 1, 0, 0.6, 0.8 );
	assert.strictEqual( result, zx );
	assertArrayClose( Array.from( zx ), [ 1.0, 2.0 ], 'zx unchanged' );
	assertArrayClose( Array.from( zy ), [ 3.0, 4.0 ], 'zy unchanged' );
});

test( 'zdrot: basic rotation (N=3, c=0.6, s=0.8)', function t() {
	// zx = [(1+2i), (3+4i), (5+6i)]
	// zy = [(7+8i), (9+10i), (11+12i)]
	// c = 0.6, s = 0.8
	//
	// zx_new[k] = c*zx[k] + s*zy[k]
	// zy_new[k] = c*zy[k] - s*zx[k]
	//
	// k=0: zx = 0.6*(1+2i) + 0.8*(7+8i) = (0.6+1.2i) + (5.6+6.4i) = (6.2+7.6i)
	//      zy = 0.6*(7+8i) - 0.8*(1+2i) = (4.2+4.8i) - (0.8+1.6i) = (3.4+3.2i)
	// k=1: zx = 0.6*(3+4i) + 0.8*(9+10i) = (1.8+2.4i) + (7.2+8.0i) = (9.0+10.4i)
	//      zy = 0.6*(9+10i) - 0.8*(3+4i) = (5.4+6.0i) - (2.4+3.2i) = (3.0+2.8i)
	// k=2: zx = 0.6*(5+6i) + 0.8*(11+12i) = (3.0+3.6i) + (8.8+9.6i) = (11.8+13.2i)
	//      zy = 0.6*(11+12i) - 0.8*(5+6i) = (6.6+7.2i) - (4.0+4.8i) = (2.6+2.4i)
	var zx = new Float64Array( [ 1.0, 2.0, 3.0, 4.0, 5.0, 6.0 ] );
	var zy = new Float64Array( [ 7.0, 8.0, 9.0, 10.0, 11.0, 12.0 ] );
	var result = base( 3, zx, 1, 0, zy, 1, 0, 0.6, 0.8 );
	assert.strictEqual( result, zx );
	assertArrayClose( Array.from( zx ), [ 6.2, 7.6, 9.0, 10.4, 11.8, 13.2 ], 'zx' );
	assertArrayClose( Array.from( zy ), [ 3.4, 3.2, 3.0, 2.8, 2.6, 2.4 ], 'zy' );
});

test( 'zdrot: identity rotation (c=1, s=0)', function t() {
	// c=1, s=0 means: zx_new = zx, zy_new = zy (identity)
	var zx = new Float64Array( [ 1.0, 2.0, 3.0, 4.0 ] );
	var zy = new Float64Array( [ 5.0, 6.0, 7.0, 8.0 ] );
	base( 2, zx, 1, 0, zy, 1, 0, 1.0, 0.0 );
	assertArrayClose( Array.from( zx ), [ 1.0, 2.0, 3.0, 4.0 ], 'zx unchanged' );
	assertArrayClose( Array.from( zy ), [ 5.0, 6.0, 7.0, 8.0 ], 'zy unchanged' );
});

test( 'zdrot: swap rotation (c=0, s=1)', function t() {
	// c=0, s=1 means: zx_new = zy, zy_new = -zx
	var zx = new Float64Array( [ 1.0, 2.0, 3.0, 4.0 ] );
	var zy = new Float64Array( [ 5.0, 6.0, 7.0, 8.0 ] );
	base( 2, zx, 1, 0, zy, 1, 0, 0.0, 1.0 );
	assertArrayClose( Array.from( zx ), [ 5.0, 6.0, 7.0, 8.0 ], 'zx = old zy' );
	assertArrayClose( Array.from( zy ), [ -1.0, -2.0, -3.0, -4.0 ], 'zy = -old zx' );
});

test( 'zdrot: N=1 single element', function t() {
	// zx = (3+4i), zy = (1+2i), c=0.6, s=0.8
	// zx_new = 0.6*(3+4i) + 0.8*(1+2i) = (1.8+2.4i) + (0.8+1.6i) = (2.6+4.0i)
	// zy_new = 0.6*(1+2i) - 0.8*(3+4i) = (0.6+1.2i) - (2.4+3.2i) = (-1.8-2.0i)
	var zx = new Float64Array( [ 3.0, 4.0 ] );
	var zy = new Float64Array( [ 1.0, 2.0 ] );
	base( 1, zx, 1, 0, zy, 1, 0, 0.6, 0.8 );
	assertArrayClose( Array.from( zx ), [ 2.6, 4.0 ], 'zx' );
	assertArrayClose( Array.from( zy ), [ -1.8, -2.0 ], 'zy' );
});

test( 'zdrot: non-unit stride (strideX=2, strideY=2)', function t() {
	// zx has 3 complex elements at positions 0, 2, 4 (with gaps at 1, 3)
	// zx[0] = (1+2i), zx[2] = (3+4i), zx[4] = (5+6i)
	// zy[0] = (10+20i), zy[2] = (30+40i), zy[4] = (50+60i)
	// c = 0.6, s = 0.8
	//
	// k=0: zx = 0.6*(1+2i) + 0.8*(10+20i) = (0.6+1.2i) + (8+16i) = (8.6+17.2i)
	//      zy = 0.6*(10+20i) - 0.8*(1+2i) = (6+12i) - (0.8+1.6i) = (5.2+10.4i)
	// k=1: zx = 0.6*(3+4i) + 0.8*(30+40i) = (1.8+2.4i) + (24+32i) = (25.8+34.4i)
	//      zy = 0.6*(30+40i) - 0.8*(3+4i) = (18+24i) - (2.4+3.2i) = (15.6+20.8i)
	// k=2: zx = 0.6*(5+6i) + 0.8*(50+60i) = (3+3.6i) + (40+48i) = (43+51.6i)
	//      zy = 0.6*(50+60i) - 0.8*(5+6i) = (30+36i) - (4+4.8i) = (26+31.2i)
	var zx = new Float64Array( [
		1.0, 2.0, 99.0, 99.0, 3.0, 4.0, 99.0, 99.0, 5.0, 6.0
	] );
	var zy = new Float64Array( [
		10.0, 20.0, 88.0, 88.0, 30.0, 40.0, 88.0, 88.0, 50.0, 60.0
	] );
	base( 3, zx, 2, 0, zy, 2, 0, 0.6, 0.8 );
	// Gap elements should be untouched
	assertClose( zx[ 2 ], 99.0, 'zx gap[2]' );
	assertClose( zx[ 3 ], 99.0, 'zx gap[3]' );
	assertClose( zx[ 6 ], 99.0, 'zx gap[6]' );
	assertClose( zx[ 7 ], 99.0, 'zx gap[7]' );
	assertClose( zy[ 2 ], 88.0, 'zy gap[2]' );
	assertClose( zy[ 3 ], 88.0, 'zy gap[3]' );
	assertClose( zy[ 6 ], 88.0, 'zy gap[6]' );
	assertClose( zy[ 7 ], 88.0, 'zy gap[7]' );
	// Check rotated elements
	assertArrayClose(
		[ zx[ 0 ], zx[ 1 ], zx[ 4 ], zx[ 5 ], zx[ 8 ], zx[ 9 ] ],
		[ 8.6, 17.2, 25.8, 34.4, 43.0, 51.6 ],
		'zx rotated'
	);
	assertArrayClose(
		[ zy[ 0 ], zy[ 1 ], zy[ 4 ], zy[ 5 ], zy[ 8 ], zy[ 9 ] ],
		[ 5.2, 10.4, 15.6, 20.8, 26.0, 31.2 ],
		'zy rotated'
	);
});

test( 'zdrot: negative stride (strideX=-1, strideY=1)', function t() {
	// With strideX=-1 and offsetX=4, we start at Float64 index 4 and go backwards
	// zx array: [1, 2, 3, 4, 5, 6] => complex elements at indices 0,1,2
	// With stride=-1 and offset=4, element 0 is at Float64[4:5]=(5+6i),
	// element 1 at Float64[2:3]=(3+4i), element 2 at Float64[0:1]=(1+2i)
	//
	// zy array: [10, 20, 30, 40, 50, 60], stride=1, offset=0
	// element 0 at Float64[0:1]=(10+20i), element 1 at [2:3]=(30+40i), element 2 at [4:5]=(50+60i)
	//
	// c = 0.6, s = 0.8
	//
	// k=0: zx_elem=(5+6i), zy_elem=(10+20i)
	//   zx_new = 0.6*(5+6i) + 0.8*(10+20i) = (3+3.6i) + (8+16i) = (11+19.6i)
	//   zy_new = 0.6*(10+20i) - 0.8*(5+6i) = (6+12i) - (4+4.8i) = (2+7.2i)
	//
	// k=1: zx_elem=(3+4i), zy_elem=(30+40i)
	//   zx_new = 0.6*(3+4i) + 0.8*(30+40i) = (1.8+2.4i) + (24+32i) = (25.8+34.4i)
	//   zy_new = 0.6*(30+40i) - 0.8*(3+4i) = (18+24i) - (2.4+3.2i) = (15.6+20.8i)
	//
	// k=2: zx_elem=(1+2i), zy_elem=(50+60i)
	//   zx_new = 0.6*(1+2i) + 0.8*(50+60i) = (0.6+1.2i) + (40+48i) = (40.6+49.2i)
	//   zy_new = 0.6*(50+60i) - 0.8*(1+2i) = (30+36i) - (0.8+1.6i) = (29.2+34.4i)
	var zx = new Float64Array( [ 1.0, 2.0, 3.0, 4.0, 5.0, 6.0 ] );
	var zy = new Float64Array( [ 10.0, 20.0, 30.0, 40.0, 50.0, 60.0 ] );
	base( 3, zx, -1, 4, zy, 1, 0, 0.6, 0.8 );
	// zx in memory (reversed stride):
	// index 4,5 (k=0) = (11, 19.6)
	// index 2,3 (k=1) = (25.8, 34.4)
	// index 0,1 (k=2) = (40.6, 49.2)
	assertArrayClose( Array.from( zx ), [ 40.6, 49.2, 25.8, 34.4, 11.0, 19.6 ], 'zx' );
	assertArrayClose( Array.from( zy ), [ 2.0, 7.2, 15.6, 20.8, 29.2, 34.4 ], 'zy' );
});

test( 'zdrot: offset (strideX=1, offsetX=2)', function t() {
	// zx has a padding element at the start, real data starts at offset 2
	// zx = [99, 99, 1, 2, 3, 4] => complex elements: (1+2i), (3+4i) starting at offset 2
	// zy = [5, 6, 7, 8] => complex elements: (5+6i), (7+8i) starting at offset 0
	// c = 0.6, s = 0.8
	//
	// k=0: zx=(1+2i), zy=(5+6i)
	//   zx_new = 0.6*(1+2i) + 0.8*(5+6i) = (0.6+1.2i) + (4+4.8i) = (4.6+6.0i)
	//   zy_new = 0.6*(5+6i) - 0.8*(1+2i) = (3+3.6i) - (0.8+1.6i) = (2.2+2.0i)
	//
	// k=1: zx=(3+4i), zy=(7+8i)
	//   zx_new = 0.6*(3+4i) + 0.8*(7+8i) = (1.8+2.4i) + (5.6+6.4i) = (7.4+8.8i)
	//   zy_new = 0.6*(7+8i) - 0.8*(3+4i) = (4.2+4.8i) - (2.4+3.2i) = (1.8+1.6i)
	var zx = new Float64Array( [ 99.0, 99.0, 1.0, 2.0, 3.0, 4.0 ] );
	var zy = new Float64Array( [ 5.0, 6.0, 7.0, 8.0 ] );
	base( 2, zx, 1, 2, zy, 1, 0, 0.6, 0.8 );
	// Padding should be untouched
	assertClose( zx[ 0 ], 99.0, 'zx pad[0]' );
	assertClose( zx[ 1 ], 99.0, 'zx pad[1]' );
	assertArrayClose(
		[ zx[ 2 ], zx[ 3 ], zx[ 4 ], zx[ 5 ] ],
		[ 4.6, 6.0, 7.4, 8.8 ],
		'zx rotated'
	);
	assertArrayClose( Array.from( zy ), [ 2.2, 2.0, 1.8, 1.6 ], 'zy' );
});

test( 'zdrot: 45-degree rotation (c=s=sqrt(2)/2)', function t() {
	// c = s = sqrt(2)/2 ~ 0.7071067811865476
	// zx = [(1+0i)], zy = [(0+1i)]
	// zx_new = c*(1+0i) + s*(0+1i) = (c + si) = (0.7071 + 0.7071i)
	// zy_new = c*(0+1i) - s*(1+0i) = (ci - s) = (-0.7071 + 0.7071i)
	var SQRT2_2 = Math.sqrt( 2.0 ) / 2.0;
	var zx = new Float64Array( [ 1.0, 0.0 ] );
	var zy = new Float64Array( [ 0.0, 1.0 ] );
	base( 1, zx, 1, 0, zy, 1, 0, SQRT2_2, SQRT2_2 );
	assertArrayClose( Array.from( zx ), [ SQRT2_2, SQRT2_2 ], 'zx' );
	assertArrayClose( Array.from( zy ), [ -SQRT2_2, SQRT2_2 ], 'zy' );
});

test( 'zdrot: returns zx', function t() {
	var zx = new Float64Array( [ 1.0, 2.0, 3.0, 4.0 ] );
	var zy = new Float64Array( [ 5.0, 6.0, 7.0, 8.0 ] );
	var result = base( 2, zx, 1, 0, zy, 1, 0, 0.6, 0.8 );
	assert.strictEqual( result, zx );
});

test( 'zdrot: ndarray method works', function t() {
	// Test the ndarray export path
	var zx = new Float64Array( [ 1.0, 2.0, 3.0, 4.0, 5.0, 6.0 ] );
	var zy = new Float64Array( [ 7.0, 8.0, 9.0, 10.0, 11.0, 12.0 ] );
	var result = zdrot.ndarray( 3, zx, 1, 0, zy, 1, 0, 0.6, 0.8 );
	assert.strictEqual( result, zx );
	assertArrayClose( Array.from( zx ), [ 6.2, 7.6, 9.0, 10.4, 11.8, 13.2 ], 'zx' );
	assertArrayClose( Array.from( zy ), [ 3.4, 3.2, 3.0, 2.8, 2.6, 2.4 ], 'zy' );
});

test( 'zdrot: mixed strides (strideX=1, strideY=2)', function t() {
	// zx stride=1: elements at Float64 positions 0, 2, 4
	// zy stride=2: elements at Float64 positions 0, 4, 8
	// zx = [(1+2i), (3+4i), (5+6i)]
	// zy = [(10+20i), gap, (30+40i), gap, (50+60i)]
	// c = 0.6, s = 0.8
	//
	// k=0: zx=(1+2i), zy=(10+20i)
	//   zx_new = 0.6*(1+2i)+0.8*(10+20i) = (0.6+1.2i)+(8+16i) = (8.6+17.2i)
	//   zy_new = 0.6*(10+20i)-0.8*(1+2i) = (6+12i)-(0.8+1.6i) = (5.2+10.4i)
	// k=1: zx=(3+4i), zy=(30+40i)
	//   zx_new = 0.6*(3+4i)+0.8*(30+40i) = (1.8+2.4i)+(24+32i) = (25.8+34.4i)
	//   zy_new = 0.6*(30+40i)-0.8*(3+4i) = (18+24i)-(2.4+3.2i) = (15.6+20.8i)
	// k=2: zx=(5+6i), zy=(50+60i)
	//   zx_new = 0.6*(5+6i)+0.8*(50+60i) = (3+3.6i)+(40+48i) = (43+51.6i)
	//   zy_new = 0.6*(50+60i)-0.8*(5+6i) = (30+36i)-(4+4.8i) = (26+31.2i)
	var zx = new Float64Array( [ 1.0, 2.0, 3.0, 4.0, 5.0, 6.0 ] );
	var zy = new Float64Array( [
		10.0, 20.0, 77.0, 77.0, 30.0, 40.0, 77.0, 77.0, 50.0, 60.0
	] );
	base( 3, zx, 1, 0, zy, 2, 0, 0.6, 0.8 );
	assertArrayClose( Array.from( zx ), [ 8.6, 17.2, 25.8, 34.4, 43.0, 51.6 ], 'zx' );
	// Gap elements should be untouched
	assertClose( zy[ 2 ], 77.0, 'zy gap[2]' );
	assertClose( zy[ 3 ], 77.0, 'zy gap[3]' );
	assertClose( zy[ 6 ], 77.0, 'zy gap[6]' );
	assertClose( zy[ 7 ], 77.0, 'zy gap[7]' );
	assertArrayClose(
		[ zy[ 0 ], zy[ 1 ], zy[ 4 ], zy[ 5 ], zy[ 8 ], zy[ 9 ] ],
		[ 5.2, 10.4, 15.6, 20.8, 26.0, 31.2 ],
		'zy rotated'
	);
});
