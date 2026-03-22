'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var Float64Array = require( '@stdlib/array/float64' );
var dsymv = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dsymv.jsonl' ), 'utf8' ).trim().split( '\n' );
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


// TESTS //

test( 'dsymv: upper_basic (uplo=U, N=4, alpha=1, beta=0, unit strides)', function t() {
	var tc = findCase( 'upper_basic' );
	// Symmetric matrix upper triangle stored in column-major:
	// Full: [[1,2,3,4],[2,5,6,7],[3,6,8,9],[4,7,9,10]]
	var A = new Float64Array([
		1, 0, 0, 0,
		2, 5, 0, 0,
		3, 6, 8, 0,
		4, 7, 9, 10
	]);
	var x = new Float64Array([ 1, 2, 3, 4 ]);
	var y = new Float64Array([ 0, 0, 0, 0 ]);

	dsymv( 'U', 4, 1.0, A, 1, 4, 0, x, 1, 0, 0.0, y, 1, 0 );
	assertArrayClose( y, tc.y, 1e-14, 'y' );
});

test( 'dsymv: lower_basic (uplo=L, N=4, alpha=1, beta=0, unit strides)', function t() {
	var tc = findCase( 'lower_basic' );
	// Lower triangle stored in column-major:
	var A = new Float64Array([
		1, 2, 3, 4,
		0, 5, 6, 7,
		0, 0, 8, 9,
		0, 0, 0, 10
	]);
	var x = new Float64Array([ 1, 2, 3, 4 ]);
	var y = new Float64Array([ 0, 0, 0, 0 ]);

	dsymv( 'L', 4, 1.0, A, 1, 4, 0, x, 1, 0, 0.0, y, 1, 0 );
	assertArrayClose( y, tc.y, 1e-14, 'y' );
});

test( 'dsymv: alpha_beta (uplo=U, alpha=2, beta=0.5)', function t() {
	var tc = findCase( 'alpha_beta' );
	var A = new Float64Array([
		1, 0, 0, 0,
		2, 5, 0, 0,
		3, 6, 8, 0,
		4, 7, 9, 10
	]);
	var x = new Float64Array([ 1, 2, 3, 4 ]);
	var y = new Float64Array([ 10, 20, 30, 40 ]);

	dsymv( 'U', 4, 2.0, A, 1, 4, 0, x, 1, 0, 0.5, y, 1, 0 );
	assertArrayClose( y, tc.y, 1e-14, 'y' );
});

test( 'dsymv: n_zero (quick return)', function t() {
	var tc = findCase( 'n_zero' );
	var A = new Float64Array([ 1 ]);
	var x = new Float64Array([ 1 ]);
	var y = new Float64Array([ 99 ]);

	dsymv( 'U', 0, 1.0, A, 1, 1, 0, x, 1, 0, 0.0, y, 1, 0 );
	assertArrayClose( y, tc.y, 1e-14, 'y' );
});

test( 'dsymv: n_one (N=1, alpha=2, beta=3)', function t() {
	var tc = findCase( 'n_one' );
	var A = new Float64Array([ 3 ]);
	var x = new Float64Array([ 5 ]);
	var y = new Float64Array([ 7 ]);

	// y = 2*3*5 + 3*7 = 30 + 21 = 51
	dsymv( 'U', 1, 2.0, A, 1, 1, 0, x, 1, 0, 3.0, y, 1, 0 );
	assertArrayClose( y, tc.y, 1e-14, 'y' );
});

test( 'dsymv: alpha_zero (alpha=0, just scales y by beta)', function t() {
	var tc = findCase( 'alpha_zero' );
	var A = new Float64Array([ 1, 0, 0, 0, 2, 5, 0, 0, 3, 6, 8, 0, 4, 7, 9, 10 ]);
	var x = new Float64Array([ 1, 2, 3, 4 ]);
	var y = new Float64Array([ 10, 20, 30, 40 ]);

	dsymv( 'U', 4, 0.0, A, 1, 4, 0, x, 1, 0, 2.0, y, 1, 0 );
	assertArrayClose( y, tc.y, 1e-14, 'y' );
});

test( 'dsymv: stride (uplo=U, N=3, incx=2, incy=2)', function t() {
	var tc = findCase( 'stride' );
	// Fortran upper triangle, LDA=3, N=3:
	// diagonal: A(1,1)=1, A(2,2)=2, A(3,3)=3, off-diags all 0
	// Column-major storage: [1,0,0, 0,2,0, 0,0,3]
	var A = new Float64Array([
		1, 0, 0,
		0, 2, 0,
		0, 0, 3
	]);
	var x = new Float64Array([ 1, 0, 2, 0, 3, 0 ]);
	var y = new Float64Array([ 1, 0, 2, 0, 3, 0 ]);

	dsymv( 'U', 3, 1.0, A, 1, 3, 0, x, 2, 0, 1.0, y, 2, 0 );
	assertArrayClose( y, tc.y, 1e-14, 'y' );
});

test( 'dsymv: lower_stride_alpha_beta (uplo=L, N=3, incx=2, incy=2, alpha=2, beta=0.5)', function t() {
	var tc = findCase( 'lower_stride_alpha_beta' );
	// Fortran lower triangle, LDA=3, N=3:
	// A(1,1)=1, A(2,1)=2, A(3,1)=3, A(2,2)=0, A(3,2)=4, A(3,3)=0
	// Symmetric: [[1,2,3],[2,0,4],[3,4,0]]
	// Column-major storage: [1,2,3, 0,0,4, 0,0,0]
	var A = new Float64Array([
		1, 2, 3,
		0, 0, 4,
		0, 0, 0
	]);
	var x = new Float64Array([ 1, 0, 2, 0, 3, 0 ]);
	var y = new Float64Array([ 10, 0, 20, 0, 30, 0 ]);

	dsymv( 'L', 3, 2.0, A, 1, 3, 0, x, 2, 0, 0.5, y, 2, 0 );
	assertArrayClose( y, tc.y, 1e-14, 'y' );
});

test( 'dsymv: negative_stride (uplo=U, N=3, incx=-1, incy=-1)', function t() {
	var tc = findCase( 'negative_stride' );
	// Upper triangle: [[1,2,3],[2,4,5],[3,5,6]]
	var A = new Float64Array([
		1, 0, 0,
		2, 4, 0,
		3, 5, 6
	]);
	var x = new Float64Array([ 1, 2, 3 ]);
	var y = new Float64Array([ 0, 0, 0 ]);

	// With incx=-1, Fortran KX = 1-(N-1)*(-1) = 3 → 0-based: offsetX = 2, strideX = -1
	dsymv( 'U', 3, 1.0, A, 1, 3, 0, x, -1, 2, 0.0, y, -1, 2 );
	assertArrayClose( y, tc.y, 1e-14, 'y' );
});

test( 'dsymv: returns y', function t() {
	var A = new Float64Array([ 1 ]);
	var x = new Float64Array([ 1 ]);
	var y = new Float64Array([ 0 ]);

	var result = dsymv( 'U', 1, 1.0, A, 1, 1, 0, x, 1, 0, 0.0, y, 1, 0 );
	assert.equal( result, y );
});

test( 'dsymv: alpha=0 and beta=1 quick return does not modify y', function t() {
	var A = new Float64Array([ 1, 2, 2, 3 ]);
	var x = new Float64Array([ 1, 2 ]);
	var y = new Float64Array([ 99, 88 ]);

	dsymv( 'U', 2, 0.0, A, 1, 2, 0, x, 1, 0, 1.0, y, 1, 0 );
	assert.equal( y[ 0 ], 99 );
	assert.equal( y[ 1 ], 88 );
});
