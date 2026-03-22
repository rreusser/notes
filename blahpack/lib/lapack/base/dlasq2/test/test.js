
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
