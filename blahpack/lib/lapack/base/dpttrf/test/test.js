

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var dpttrf = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dpttrf.jsonl' ), 'utf8' ).trim().split( '\n' );
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

test( 'dpttrf: basic_5x5', function t() {
	var info;
	var tc = findCase( 'basic_5x5' );
	var d = new Float64Array( [ 4.0, 4.0, 4.0, 4.0, 4.0 ] );
	var e = new Float64Array( [ -1.0, -1.0, -1.0, -1.0 ] );

	info = dpttrf( 5, d, 1, 0, e, 1, 0 );

	assert.equal( info, tc.info, 'info' );
	assertArrayClose( d, tc.d, 1e-14, 'd' );
	assertArrayClose( e, tc.e, 1e-14, 'e' );
});

test( 'dpttrf: n_one', function t() {
	var info;
	var tc = findCase( 'n_one' );
	var d = new Float64Array( [ 3.0 ] );
	var e = new Float64Array( [] );

	info = dpttrf( 1, d, 1, 0, e, 1, 0 );

	assert.equal( info, tc.info, 'info' );
	assertArrayClose( d, tc.d, 1e-14, 'd' );
});

test( 'dpttrf: n_zero', function t() {
	var info;
	var tc = findCase( 'n_zero' );
	var d = new Float64Array( [] );
	var e = new Float64Array( [] );

	info = dpttrf( 0, d, 1, 0, e, 1, 0 );

	assert.equal( info, tc.info, 'info' );
});

test( 'dpttrf: not_posdef_first', function t() {
	var info;
	var tc = findCase( 'not_posdef_first' );
	var d = new Float64Array( [ -1.0, 4.0, 4.0 ] );
	var e = new Float64Array( [ 1.0, 1.0 ] );

	info = dpttrf( 3, d, 1, 0, e, 1, 0 );

	assert.equal( info, tc.info, 'info' );
	// d and e should be unchanged since failure at first element
	assertArrayClose( d, tc.d, 1e-14, 'd' );
	assertArrayClose( e, tc.e, 1e-14, 'e' );
});

test( 'dpttrf: not_posdef_mid', function t() {
	var info;
	var tc = findCase( 'not_posdef_mid' );
	var d = new Float64Array( [ 1.0, 1.0, 4.0 ] );
	var e = new Float64Array( [ 2.0, 1.0 ] );

	info = dpttrf( 3, d, 1, 0, e, 1, 0 );

	assert.equal( info, tc.info, 'info' );
	assertArrayClose( d, tc.d, 1e-14, 'd' );
	assertArrayClose( e, tc.e, 1e-14, 'e' );
});

test( 'dpttrf: unrolled_8x8', function t() {
	var info;
	var tc = findCase( 'unrolled_8x8' );
	var d = new Float64Array( [ 5.0, 5.0, 5.0, 5.0, 5.0, 5.0, 5.0, 5.0 ] );
	var e = new Float64Array( [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ] );

	info = dpttrf( 8, d, 1, 0, e, 1, 0 );

	assert.equal( info, tc.info, 'info' );
	assertArrayClose( d, tc.d, 1e-14, 'd' );
	assertArrayClose( e, tc.e, 1e-14, 'e' );
});

test( 'dpttrf: n_two', function t() {
	var info;
	var tc = findCase( 'n_two' );
	var d = new Float64Array( [ 4.0, 4.0 ] );
	var e = new Float64Array( [ 2.0 ] );

	info = dpttrf( 2, d, 1, 0, e, 1, 0 );

	assert.equal( info, tc.info, 'info' );
	assertArrayClose( d, tc.d, 1e-14, 'd' );
	assertArrayClose( e, tc.e, 1e-14, 'e' );
});

test( 'dpttrf: non-unit stride', function t() {
	var info;
	// Test with stride=2 and offset=1 (same basic 5x5 case but embedded in larger arrays)
	var tc = findCase( 'basic_5x5' );
	var d = new Float64Array( [ 0.0, 4.0, 0.0, 4.0, 0.0, 4.0, 0.0, 4.0, 0.0, 4.0 ] );
	var e = new Float64Array( [ 0.0, -1.0, 0.0, -1.0, 0.0, -1.0, 0.0, -1.0 ] );

	info = dpttrf( 5, d, 2, 1, e, 2, 1 );

	assert.equal( info, tc.info, 'info' );
	var i;
	for ( i = 0; i < 5; i++ ) {
		assertClose( d[ 1 + ( i * 2 ) ], tc.d[ i ], 1e-14, 'd[' + i + ']' );
	}
	for ( i = 0; i < 4; i++ ) {
		assertClose( e[ 1 + ( i * 2 ) ], tc.e[ i ], 1e-14, 'e[' + i + ']' );
	}
});

test( 'dpttrf: negative N returns 0', function t() {
	var d = new Float64Array( [ 1.0 ] );
	var e = new Float64Array( [] );
	var info = dpttrf( -1, d, 1, 0, e, 1, 0 );
	assert.equal( info, 0, 'negative N returns 0' );
});

test( 'dpttrf: fail at position 1 in unrolled loop (N=5)', function t() {
	// N=5, i4=(5-1)%4=0, unrolled loop starts at i=0.
	// Fail at i=0 (info=1): d[0] <= 0
	var d = new Float64Array( [ 0.0, 4.0, 4.0, 4.0, 4.0 ] );
	var e = new Float64Array( [ 1.0, 1.0, 1.0, 1.0 ] );
	var info = dpttrf( 5, d, 1, 0, e, 1, 0 );
	assert.equal( info, 1, 'info=1 at start of unrolled block' );
});

test( 'dpttrf: fail at position 2 in unrolled loop (N=5)', function t() {
	// N=5, i4=0. d[0]=1, e[0]=2 => d[1]=4-(2/1)*2=0 => fail at i+1=2
	var d = new Float64Array( [ 1.0, 4.0, 4.0, 4.0, 4.0 ] );
	var e = new Float64Array( [ 2.0, 1.0, 1.0, 1.0 ] );
	var info = dpttrf( 5, d, 1, 0, e, 1, 0 );
	assert.equal( info, 2, 'info=2 at position i+1 in unrolled block' );
});

test( 'dpttrf: fail at position 3 in unrolled loop (N=5)', function t() {
	// N=5, i4=0. Need d[2]<=0 after factorization.
	// d[0]=1, e[0]=0.5 => e[0]=0.5, d[1]=4-0.25=3.75
	// d[1]=3.75, e[1]=4 => e[1]=4/3.75=1.0667, d[2]=1-1.0667*4=-3.267 => fail at 3
	var d = new Float64Array( [ 1.0, 4.0, 1.0, 4.0, 4.0 ] );
	var e = new Float64Array( [ 0.5, 4.0, 1.0, 1.0 ] );
	var info = dpttrf( 5, d, 1, 0, e, 1, 0 );
	assert.equal( info, 3, 'info=3 at position i+2 in unrolled block' );
});

test( 'dpttrf: fail at position 4 in unrolled loop (N=5)', function t() {
	// N=5, i4=0. Need d[3]<=0 after factorization.
	// d=[4,4,4,1,4], e=[0.1,0.1,4,1]: first 3 steps are fine,
	// d[0]=4, e[0]=0.1/4=0.025, d[1]=4-0.025*0.1=3.9975
	// d[1]=3.9975, e[1]=0.1/3.9975~0.02502, d[2]=4-0.02502*0.1~3.9975
	// d[2]~3.9975, e[2]=4/3.9975~1.0006, d[3]=1-1.0006*4~-3.00 => fail at 4
	var d = new Float64Array( [ 4.0, 4.0, 4.0, 1.0, 4.0 ] );
	var e = new Float64Array( [ 0.1, 0.1, 4.0, 1.0 ] );
	var info = dpttrf( 5, d, 1, 0, e, 1, 0 );
	assert.equal( info, 4, 'info=4 at position i+3 in unrolled block' );
});

test( 'dpttrf: d(N) not positive definite (last element)', function t() {
	// N=5, all steps succeed but d[4] ends up <= 0
	// d=[4,4,4,4,1], e=[0.1,0.1,0.1,4]
	// Step 0: d[0]=4, e[0]=0.025, d[1]=4-0.0025=3.9975
	// Step 1: d[1]=3.9975, e[1]~0.02502, d[2]=4-0.002502~3.9975
	// Step 2: d[2]~3.9975, e[2]~0.02502, d[3]=4-0.002502~3.9975
	// Step 3: d[3]~3.9975, e[3]=4/3.9975~1.0006, d[4]=1-1.0006*4~-3.00 => d(N)<=0
	var d = new Float64Array( [ 4.0, 4.0, 4.0, 4.0, 1.0 ] );
	var e = new Float64Array( [ 0.1, 0.1, 0.1, 4.0 ] );
	var info = dpttrf( 5, d, 1, 0, e, 1, 0 );
	assert.equal( info, 5, 'info=N when d(N) <= 0' );
});
