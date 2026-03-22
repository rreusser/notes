
'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var dlasq2 = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dlasq2.jsonl' ), 'utf8' ).trim().split( '\n' );
var fixture = lines.map( function parse( line ) { return JSON.parse( line ); } );


// FUNCTIONS //

function findCase( name ) {
	return fixture.find( function find( t ) { return t.name === name; } );
}

function assertClose( actual, expected, tol, msg ) {
	if ( expected === 0.0 ) {
		assert.ok( Math.abs( actual ) <= tol, msg + ': expected ' + expected + ', got ' + actual );
		return;
	}
	var relErr = Math.abs( actual - expected ) / Math.max( Math.abs( expected ), 1.0 );
	assert.ok( relErr <= tol, msg + ': expected ' + expected + ', got ' + actual );
}

function assertArrayClose( actual, expected, tol, msg ) {
	var i;
	for ( i = 0; i < expected.length; i++ ) {
		assertClose( actual[ i ], expected[ i ], tol, msg + '[' + i + ']' );
	}
}

/**
* Creates a Float64Array of given size, optionally initialized from an object
* mapping 1-based indices to values.
*/
function createZ( size, vals ) {
	var z = new Float64Array( size );
	var k;
	if ( vals ) {
		for ( k = 0; k < vals.length; k++ ) {
			z[ k ] = vals[ k ];
		}
	}
	return z;
}


// TESTS //

test( 'dlasq2: n0 - quick return', function t() {
	var tc = findCase( 'n0' );
	var z = createZ( 4 );
	var info = dlasq2( 0, z, 1, 0 );
	assert.strictEqual( info, tc.info );
});

test( 'dlasq2: n1 - single element', function t() {
	var tc = findCase( 'n1' );
	var z = createZ( 100, [ 4.0 ] );
	var info = dlasq2( 1, z, 1, 0 );
	assert.strictEqual( info, tc.info );
	assertArrayClose( z, tc.z, 1e-14, 'z' );
});

test( 'dlasq2: n2 - basic 2-by-2', function t() {
	var tc = findCase( 'n2' );
	var z = createZ( 100, [ 4.0, 1.0, 3.0 ] );
	var info = dlasq2( 2, z, 1, 0 );
	assert.strictEqual( info, tc.info );
	assertArrayClose( z, tc.z, 1e-14, 'z' );
});

test( 'dlasq2: n2_swap - swap case', function t() {
	var tc = findCase( 'n2_swap' );
	var z = createZ( 100, [ 2.0, 1.0, 5.0 ] );
	var info = dlasq2( 2, z, 1, 0 );
	assert.strictEqual( info, tc.info );
	assertArrayClose( z, tc.z, 1e-14, 'z' );
});

test( 'dlasq2: n3_basic', function t() {
	var tc = findCase( 'n3_basic' );
	var z = createZ( 100, [ 4.0, 1.0, 3.0, 0.5, 2.0 ] );
	var info = dlasq2( 3, z, 1, 0 );
	assert.strictEqual( info, tc.info );
	assertArrayClose( z, tc.z, 1e-14, 'z' );
});

test( 'dlasq2: n4_basic', function t() {
	var tc = findCase( 'n4_basic' );
	var z = createZ( 100, [ 4.0, 1.0, 3.0, 0.5, 5.0, 0.3, 2.0 ] );
	var info = dlasq2( 4, z, 1, 0 );
	assert.strictEqual( info, tc.info );
	assertArrayClose( z, tc.z, 1e-14, 'z' );
});

test( 'dlasq2: n3_diagonal - all off-diag zero', function t() {
	var tc = findCase( 'n3_diagonal' );
	var z = createZ( 100, [ 4.0, 0.0, 3.0, 0.0, 2.0 ] );
	var info = dlasq2( 3, z, 1, 0 );
	assert.strictEqual( info, tc.info );
	assertArrayClose( z, tc.z, 1e-14, 'z' );
});

test( 'dlasq2: n5_basic', function t() {
	var tc = findCase( 'n5_basic' );
	var z = createZ( 100, [ 9.0, 1.0, 8.0, 0.5, 7.0, 0.3, 6.0, 0.2, 5.0 ] );
	var info = dlasq2( 5, z, 1, 0 );
	assert.strictEqual( info, tc.info );
	assertArrayClose( z, tc.z, 1e-14, 'z' );
});

test( 'dlasq2: n3_reversal - triggers reversal', function t() {
	var tc = findCase( 'n3_reversal' );
	var z = createZ( 100, [ 1.0, 0.5, 2.0, 0.3, 10.0 ] );
	var info = dlasq2( 3, z, 1, 0 );
	assert.strictEqual( info, tc.info );
	assertArrayClose( z, tc.z, 1e-14, 'z' );
});

test( 'dlasq2: n4_identity_like', function t() {
	var tc = findCase( 'n4_identity_like' );
	var z = createZ( 100, [ 1.0, 0.1, 1.0, 0.1, 1.0, 0.1, 1.0 ] );
	var info = dlasq2( 4, z, 1, 0 );
	assert.strictEqual( info, tc.info );
	assertArrayClose( z, tc.z, 1e-14, 'z' );
});

test( 'dlasq2: n2_zero_offdiag', function t() {
	var tc = findCase( 'n2_zero_offdiag' );
	var z = createZ( 100, [ 4.0, 0.0, 3.0 ] );
	var info = dlasq2( 2, z, 1, 0 );
	assert.strictEqual( info, tc.info );
	assertArrayClose( z, tc.z, 1e-14, 'z' );
});

// Error cases (not in Fortran fixtures, but test JS behavior)
test( 'dlasq2: negative N returns -1', function t() {
	var z = createZ( 4 );
	var info = dlasq2( -1, z, 1, 0 );
	assert.strictEqual( info, -1 );
});

test( 'dlasq2: N=1 negative z returns -201', function t() {
	var z = createZ( 100, [ -1.0 ] );
	var info = dlasq2( 1, z, 1, 0 );
	assert.strictEqual( info, -201 );
});

test( 'dlasq2: N=2 negative z(2) returns -202', function t() {
	var z = createZ( 100, [ 4.0, -1.0, 3.0 ] );
	var info = dlasq2( 2, z, 1, 0 );
	assert.strictEqual( info, -202 );
});

test( 'dlasq2: N=2 negative z(3) returns -203', function t() {
	var z = createZ( 100, [ 4.0, 1.0, -3.0 ] );
	var info = dlasq2( 2, z, 1, 0 );
	assert.strictEqual( info, -203 );
});

test( 'dlasq2: N=2 negative z(1) returns -201', function t() {
	var z = createZ( 100, [ -4.0, 1.0, 3.0 ] );
	var info = dlasq2( 2, z, 1, 0 );
	assert.strictEqual( info, -201 );
});

test( 'dlasq2: N=2 s>t branch', function t() {
	// Z(1)~Z(3) with large Z(2) makes s>t in 2x2 eigenvalue computation
	var z = createZ( 100, [ 1.0, 10.0, 1.0 ] );
	var info = dlasq2( 2, z, 1, 0 );
	assert.strictEqual( info, 0 );
	assertClose( z[ 0 ], 11.916079783099615, 1e-14, 'd[0]' );
	assertClose( z[ 1 ], 0.08392021690038397, 1e-14, 'd[1]' );
});

test( 'dlasq2: N>=3 negative diagonal returns error', function t() {
	var z = createZ( 100, [ 4.0, 1.0, -3.0, 0.5, 2.0 ] );
	var info = dlasq2( 3, z, 1, 0 );
	assert.strictEqual( info, -203 );
});

test( 'dlasq2: N>=3 negative off-diagonal returns error', function t() {
	var z = createZ( 100, [ 4.0, -1.0, 3.0, 0.5, 2.0 ] );
	var info = dlasq2( 3, z, 1, 0 );
	assert.strictEqual( info, -202 );
});

test( 'dlasq2: N>=3 negative last diagonal returns error', function t() {
	var z = createZ( 100, [ 4.0, 1.0, 3.0, 0.5, -2.0 ] );
	var info = dlasq2( 3, z, 1, 0 );
	assert.strictEqual( info, -205 );
});

test( 'dlasq2: n5_split - matrix with zero off-diagonal (split)', function t() {
	// Off-diagonal zero creates a split in the bidiagonal matrix
	var z = createZ( 100, [ 9.0, 1.0, 8.0, 0.0, 7.0, 0.5, 6.0, 0.3, 5.0 ] );
	var info = dlasq2( 5, z, 1, 0 );
	assert.strictEqual( info, 0 );
	// Singular values should be sorted descending
	assert.ok( z[ 0 ] >= z[ 1 ], 'd[0] >= d[1]' );
	assert.ok( z[ 1 ] >= z[ 2 ], 'd[1] >= d[2]' );
});

test( 'dlasq2: n3_tiny_offdiag - nearly diagonal', function t() {
	// Very small off-diagonals trigger split detection in initial checking
	var z = createZ( 100, [ 4.0, 1e-20, 3.0, 1e-20, 2.0 ] );
	var info = dlasq2( 3, z, 1, 0 );
	assert.strictEqual( info, 0 );
	assertClose( z[ 0 ], 4.0, 1e-10, 'd[0]' );
	assertClose( z[ 1 ], 3.0, 1e-10, 'd[1]' );
	assertClose( z[ 2 ], 2.0, 1e-10, 'd[2]' );
});

test( 'dlasq2: n4_ascending - ascending diagonals trigger aggressive dqds', function t() {
	// Small values first, large last - triggers pp=2 reversal
	var z = createZ( 100, [ 1.0, 0.5, 1.0, 0.5, 1.0, 0.5, 10.0 ] );
	var info = dlasq2( 4, z, 1, 0 );
	assert.strictEqual( info, 0 );
	assert.ok( z[ 0 ] >= z[ 1 ], 'd[0] >= d[1]' );
});

test( 'dlasq2: n8_ascending - ascending pattern for aggressive dqds', function t() {
	// Strongly ascending: tiny values first, large last
	var z = createZ( 200, [
		0.1, 0.5, 0.2, 0.5, 0.3, 0.5, 0.5, 0.5,
		1.0, 0.5, 2.0, 0.5, 5.0, 0.5, 10.0
	] );
	var info = dlasq2( 8, z, 1, 0 );
	assert.strictEqual( info, 0 );
	assert.ok( z[ 0 ] >= z[ 1 ], 'd[0] >= d[1]' );
});

test( 'dlasq2: n10_ascending - larger ascending for aggressive dqds reversal', function t() {
	// Very small starting values, large ending value - strong reversal trigger
	var vals = [];
	var i;
	for ( i = 0; i < 9; i++ ) {
		vals.push( ( i + 1 ) * 0.01 );
		vals.push( 0.01 );
	}
	vals.push( 100.0 );
	var z = createZ( 200, vals );
	var info = dlasq2( 10, z, 1, 0 );
	assert.strictEqual( info, 0 );
	assert.ok( z[ 0 ] >= z[ 1 ], 'd[0] >= d[1]' );
});

test( 'dlasq2: n6_tiny_then_large - triggers aggressive dqds pp=2 reversal', function t() {
	// Tiny q values followed by a large one to trigger kmin near i0
	var z = createZ( 200, [ 0.001, 0.1, 0.001, 0.1, 0.001, 0.1, 0.001, 0.1, 0.001, 0.1, 100.0 ] );
	var info = dlasq2( 6, z, 1, 0 );
	assert.strictEqual( info, 0 );
	assert.ok( z[ 0 ] >= z[ 1 ], 'd[0] >= d[1]' );
});

test( 'dlasq2: n8_graded - graded matrix for aggressive dqds', function t() {
	// Geometrically graded: tiny values at start, growing rapidly
	var z = createZ( 200, [
		1e-8, 1e-4, 1e-6, 1e-4, 1e-4, 1e-4, 1e-2, 1e-4,
		1.0, 1e-4, 1e2, 1e-4, 1e4, 1e-4, 1e6
	] );
	var info = dlasq2( 8, z, 1, 0 );
	assert.strictEqual( info, 0 );
	assert.ok( z[ 0 ] >= z[ 1 ], 'd[0] >= d[1]' );
});
