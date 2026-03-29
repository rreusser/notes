/* eslint-disable no-restricted-syntax, stdlib/require-globals, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dtrsv = require( './../lib/base.js' );
var ndarray = require( './../lib/ndarray.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dtrsv.jsonl' ), 'utf8' ).trim().split( '\n' );
var fixture = lines.map( function parse( line ) { return JSON.parse( line ); } );


// FUNCTIONS //

function findCase( name ) {
	return fixture.find( function find( t ) { return t.name === name; } );
}

function assertClose( actual, expected, tol, msg ) {
	var relErr = Math.abs( actual - expected ) / Math.max( Math.abs( expected ), 1.0 );
	assert.ok( relErr <= tol, msg + ': expected ' + expected + ', got ' + actual );
}

function assertArrayClose( actual, expected, tol, msg ) {
	var i;
	assert.equal( actual.length, expected.length, msg + ': length mismatch' );
	for ( i = 0; i < expected.length; i++ ) {
		assertClose( actual[ i ], expected[ i ], tol, msg + '[' + i + ']' );
	}
}

// Helper: create upper triangular 3x3 matrix [2 3 4; 0 5 6; 0 0 7] in col-major
function upperA3() {
	//     col0  col1  col2
	return new Float64Array([
		2.0,
		0.0,
		0.0,  // col 0
		3.0,
		5.0,
		0.0,  // col 1
		4.0,
		6.0,
		7.0   // col 2
	]);
}

// Helper: create lower triangular 3x3 matrix [2 0 0; 3 5 0; 4 6 7] in col-major
function lowerA3() {
	return new Float64Array([
		2.0,
		3.0,
		4.0,  // col 0
		0.0,
		5.0,
		6.0,  // col 1
		0.0,
		0.0,
		7.0   // col 2
	]);
}


// TESTS //

test( 'dtrsv: upper, no-transpose, non-unit diag (N=3)', function t() {
	var tc = findCase( 'upper_n_nonunit' );
	var A = upperA3();

	// B = A * [1,2,3] = [20, 28, 21]
	var x = new Float64Array([ 20.0, 28.0, 21.0 ]);
	dtrsv( 'upper', 'no-transpose', 'non-unit', 3, A, 1, 3, 0, x, 1, 0 );
	assertArrayClose( x, tc.x, 1e-14, 'x' );
});

test( 'dtrsv: lower, no-transpose, non-unit diag (N=3)', function t() {
	var tc = findCase( 'lower_n_nonunit' );
	var A = lowerA3();

	// B = A * [1,2,3] = [2, 13, 37]
	var x = new Float64Array([ 2.0, 13.0, 37.0 ]);
	dtrsv( 'lower', 'no-transpose', 'non-unit', 3, A, 1, 3, 0, x, 1, 0 );
	assertArrayClose( x, tc.x, 1e-14, 'x' );
});

test( 'dtrsv: upper, transpose, non-unit diag (N=3)', function t() {
	var tc = findCase( 'upper_t_nonunit' );
	var A = upperA3();

	// B = A^T * [1,2,3] = [2, 13, 37]
	var x = new Float64Array([ 2.0, 13.0, 37.0 ]);
	dtrsv( 'upper', 'transpose', 'non-unit', 3, A, 1, 3, 0, x, 1, 0 );
	assertArrayClose( x, tc.x, 1e-14, 'x' );
});

test( 'dtrsv: lower, transpose, non-unit diag (N=3)', function t() {
	var tc = findCase( 'lower_t_nonunit' );
	var A = lowerA3();

	// B = A^T * [1,2,3] = [20, 28, 21]
	var x = new Float64Array([ 20.0, 28.0, 21.0 ]);
	dtrsv( 'lower', 'transpose', 'non-unit', 3, A, 1, 3, 0, x, 1, 0 );
	assertArrayClose( x, tc.x, 1e-14, 'x' );
});

test( 'dtrsv: upper, no-transpose, unit diag (N=3)', function t() {
	var tc = findCase( 'upper_n_unit' );

	// Unit diag: A = [1 3 4; 0 1 6; 0 0 1], diag values set to 99 (should be ignored)
	var A = new Float64Array([
		99.0,
		0.0,
		0.0,
		3.0,
		99.0,
		0.0,
		4.0,
		6.0,
		99.0
	]);

	// B = A*[1,2,3] = [19, 20, 3]
	var x = new Float64Array([ 19.0, 20.0, 3.0 ]);
	dtrsv( 'upper', 'no-transpose', 'unit', 3, A, 1, 3, 0, x, 1, 0 );
	assertArrayClose( x, tc.x, 1e-14, 'x' );
});

test( 'dtrsv: lower, no-transpose, unit diag (N=3)', function t() {
	var tc = findCase( 'lower_n_unit' );

	// Unit diag: A = [1 0 0; 3 1 0; 4 6 1]
	var A = new Float64Array([
		99.0,
		3.0,
		4.0,
		0.0,
		99.0,
		6.0,
		0.0,
		0.0,
		99.0
	]);

	// B = A*[1,2,3] = [1, 5, 19]
	var x = new Float64Array([ 1.0, 5.0, 19.0 ]);
	dtrsv( 'lower', 'no-transpose', 'unit', 3, A, 1, 3, 0, x, 1, 0 );
	assertArrayClose( x, tc.x, 1e-14, 'x' );
});

test( 'dtrsv: upper, transpose, unit diag (N=3)', function t() {
	var tc = findCase( 'upper_t_unit' );

	// Unit diag: A = [1 3 4; 0 1 6; 0 0 1]

	// A^T = [1 0 0; 3 1 0; 4 6 1]
	var A = new Float64Array([
		99.0,
		0.0,
		0.0,
		3.0,
		99.0,
		0.0,
		4.0,
		6.0,
		99.0
	]);

	// B = A^T*[1,2,3] = [1, 5, 19]
	var x = new Float64Array([ 1.0, 5.0, 19.0 ]);
	dtrsv( 'upper', 'transpose', 'unit', 3, A, 1, 3, 0, x, 1, 0 );
	assertArrayClose( x, tc.x, 1e-14, 'x' );
});

test( 'dtrsv: lower, transpose, unit diag (N=3)', function t() {
	var tc = findCase( 'lower_t_unit' );

	// Unit diag: A = [1 0 0; 3 1 0; 4 6 1]

	// A^T = [1 3 4; 0 1 6; 0 0 1]
	var A = new Float64Array([
		99.0,
		3.0,
		4.0,
		0.0,
		99.0,
		6.0,
		0.0,
		0.0,
		99.0
	]);

	// B = A^T*[1,2,3] = [19, 20, 3]
	var x = new Float64Array([ 19.0, 20.0, 3.0 ]);
	dtrsv( 'lower', 'transpose', 'unit', 3, A, 1, 3, 0, x, 1, 0 );
	assertArrayClose( x, tc.x, 1e-14, 'x' );
});

test( 'dtrsv: N=0 quick return', function t() {
	var out = dtrsv( 'upper', 'no-transpose', 'non-unit', 0, A, 1, 1, 0, x, 1, 0 );
	var A = new Float64Array([ 1.0 ]);
	var x = new Float64Array([ 99.0 ]);
	assert.equal( x[ 0 ], 99.0 );
	assert.equal( out, x );
});

test( 'dtrsv: N=1, non-unit diag', function t() {
	var tc = findCase( 'n_one' );
	var A = new Float64Array([ 5.0 ]);
	var x = new Float64Array([ 15.0 ]);
	dtrsv( 'upper', 'no-transpose', 'non-unit', 1, A, 1, 1, 0, x, 1, 0 );
	assertArrayClose( x, tc.x, 1e-14, 'x' );
});

test( 'dtrsv: non-unit stride (incx=2)', function t() {
	var tc = findCase( 'stride' );
	var A = upperA3();

	// B at stride 2: positions 0,2,4 hold [20,28,21]
	var x = new Float64Array([ 20.0, 0.0, 28.0, 0.0, 21.0, 0.0 ]);
	dtrsv( 'upper', 'no-transpose', 'non-unit', 3, A, 1, 3, 0, x, 2, 0 );
	assertArrayClose( x, tc.x, 1e-14, 'x' );
});

test( 'dtrsv: negative stride (incx=-1)', function t() {
	var tc = findCase( 'neg_stride' );
	var A = lowerA3();

	// With incx=-1, x stored in reverse: x[2]=b(1), x[1]=b(2), x[0]=b(3)
	var x = new Float64Array([ 37.0, 13.0, 2.0 ]);

	// Negative stride: strideX=-1, offsetX = (N-1)*|strideX| = 2
	dtrsv( 'lower', 'no-transpose', 'non-unit', 3, A, 1, 3, 0, x, -1, 2 );
	assertArrayClose( x, tc.x, 1e-14, 'x' );
});

test( 'dtrsv: upper, no-transpose, non-unit (4x4)', function t() {
	var tc = findCase( 'upper_n_nonunit_4x4' );

	// A (upper, col-major 4x4):

	//   [1  2  3  4]

	//   [0  5  6  7]

	//   [0  0  8  9]

	//   [0  0  0 10]
	var A = new Float64Array([
		1.0,
		0.0,
		0.0,
		0.0,  // col 0
		2.0,
		5.0,
		0.0,
		0.0,  // col 1
		3.0,
		6.0,
		8.0,
		0.0,  // col 2
		4.0,
		7.0,
		9.0,
		10.0  // col 3
	]);

	// B = A*[1,1,1,1] = [10, 18, 17, 10]
	var x = new Float64Array([ 10.0, 18.0, 17.0, 10.0 ]);
	dtrsv( 'upper', 'no-transpose', 'non-unit', 4, A, 1, 4, 0, x, 1, 0 );
	assertArrayClose( x, tc.x, 1e-14, 'x' );
});

test( 'dtrsv: N=1, unit diag', function t() {
	var tc = findCase( 'n_one_unit' );
	var A = new Float64Array([ 99.0 ]);
	var x = new Float64Array([ 7.0 ]);
	dtrsv( 'lower', 'transpose', 'unit', 1, A, 1, 1, 0, x, 1, 0 );
	assertArrayClose( x, tc.x, 1e-14, 'x' );
});

test( 'dtrsv: upper, no-transpose with zero RHS entries', function t() {
	var tc = findCase( 'upper_n_zeros' );
	var A = upperA3();

	// B = [0, 0, 21]
	var x = new Float64Array([ 0.0, 0.0, 21.0 ]);
	dtrsv( 'upper', 'no-transpose', 'non-unit', 3, A, 1, 3, 0, x, 1, 0 );
	assertArrayClose( x, tc.x, 1e-14, 'x' );
});

test( 'dtrsv: returns x', function t() {
	var out = dtrsv( 'upper', 'no-transpose', 'non-unit', 1, A, 1, 1, 0, x, 1, 0 );
	var A = new Float64Array([ 2.0 ]);
	var x = new Float64Array([ 4.0 ]);
	assert.equal( out, x );
});

test( 'dtrsv: with offsetA', function t() {
	// Place the 3x3 upper triangular matrix at offset 2 in A
	var A = new Float64Array([
		0.0,
		0.0,          // padding
		2.0,
		0.0,
		0.0,     // col 0
		3.0,
		5.0,
		0.0,     // col 1
		4.0,
		6.0,
		7.0      // col 2
	]);
	var x = new Float64Array([ 20.0, 28.0, 21.0 ]);
	dtrsv( 'upper', 'no-transpose', 'non-unit', 3, A, 1, 3, 2, x, 1, 0 );
	assertArrayClose( x, [ 1.0, 2.0, 3.0 ], 1e-14, 'x' );
});

test( 'dtrsv: with offsetX', function t() {
	var A = upperA3();
	var x = new Float64Array([ 0.0, 0.0, 20.0, 28.0, 21.0 ]);
	dtrsv( 'upper', 'no-transpose', 'non-unit', 3, A, 1, 3, 0, x, 1, 2 );
	assertArrayClose( [ x[2], x[3], x[4] ], [ 1.0, 2.0, 3.0 ], 1e-14, 'x' );
});


// NDARRAY VALIDATION TESTS //

test( 'ndarray: throws TypeError for invalid uplo', function t() {
	var x = new Float64Array( [ 1, 2, 3 ] );
	assert.throws( function f() {
		ndarray( 'foo', 'no-transpose', 'non-unit', 3, upperA3(), 1, 3, 0, x, 1, 0 );
	}, TypeError );
});

test( 'ndarray: throws TypeError for invalid trans', function t() {
	var x = new Float64Array( [ 1, 2, 3 ] );
	assert.throws( function f() {
		ndarray( 'upper', 'foo', 'non-unit', 3, upperA3(), 1, 3, 0, x, 1, 0 );
	}, TypeError );
});

test( 'ndarray: throws TypeError for invalid diag', function t() {
	var x = new Float64Array( [ 1, 2, 3 ] );
	assert.throws( function f() {
		ndarray( 'upper', 'no-transpose', 'foo', 3, upperA3(), 1, 3, 0, x, 1, 0 );
	}, TypeError );
});

test( 'ndarray: throws RangeError for negative N', function t() {
	var x = new Float64Array( [ 1, 2, 3 ] );
	assert.throws( function f() {
		ndarray( 'upper', 'no-transpose', 'non-unit', -1, upperA3(), 1, 3, 0, x, 1, 0 );
	}, RangeError );
});

test( 'ndarray: throws RangeError for strideX=0', function t() {
	var x = new Float64Array( [ 1, 2, 3 ] );
	assert.throws( function f() {
		ndarray( 'upper', 'no-transpose', 'non-unit', 3, upperA3(), 1, 3, 0, x, 0, 0 );
	}, RangeError );
});

test( 'ndarray: N=0 early return', function t() {
	var out = ndarray( 'upper', 'no-transpose', 'non-unit', 0, new Float64Array( 1 ), 1, 1, 0, x, 1, 0 );
	var x = new Float64Array( [ 99 ] );
	assert.equal( out, x );
	assert.equal( x[ 0 ], 99 );
});
