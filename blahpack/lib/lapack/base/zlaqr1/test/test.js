'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Complex128Array = require( '@stdlib/array/complex128' );
var Complex128 = require( '@stdlib/complex/float64/ctor' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var zlaqr1 = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'zlaqr1.jsonl' ), 'utf8' ).trim().split( '\n' );
var fixture = lines.map( function parse( line ) { return JSON.parse( line ); } );


// FUNCTIONS //

function findCase( name ) {
	return fixture.find( function find( t ) { return t.name === name; } );
}

function assertClose( actual, expected, tol, msg ) {
	var relErr = Math.abs( actual - expected ) / Math.max( Math.abs( expected ), 1.0 );
	assert.ok( relErr <= tol, msg + ': expected ' + expected + ', got ' + actual + ' (relErr=' + relErr + ')' );
}

function assertArrayClose( actual, expected, tol, msg ) {
	var i;
	assert.equal( actual.length, expected.length, msg + ': length mismatch (' + actual.length + ' vs ' + expected.length + ')' );
	for ( i = 0; i < expected.length; i++ ) {
		assertClose( actual[ i ], expected[ i ], tol, msg + '[' + i + ']' );
	}
}

/**
* Build an NxN complex column-major matrix.
*/
function makeMatrix( N ) {
	return {
		data: new Complex128Array( N * N ),
		s1: 1,
		s2: N,
		offset: 0
	};
}

/**
* Set complex element (i, j) in a matrix (0-based).
*/
function mset( m, N, i, j, re, im ) {
	var mv = reinterpret( m.data, 0 );
	var idx = ( m.offset + i * m.s1 + j * m.s2 ) * 2;
	mv[ idx ] = re;
	mv[ idx + 1 ] = im;
}


// TESTS //

test( 'zlaqr1: main export is a function', function t() {
	assert.strictEqual( typeof zlaqr1, 'function' );
});

test( 'zlaqr1: 2x2 with real shifts', function t() {
	var tc = findCase( '2x2_real_shifts' );
	var n = 2;
	var Hm = makeMatrix( n );
	var v = new Complex128Array( n );
	var vv;

	mset( Hm, n, 0, 0, 4.0, 0.0 );
	mset( Hm, n, 0, 1, 2.0, 0.0 );
	mset( Hm, n, 1, 0, 1.0, 0.0 );
	mset( Hm, n, 1, 1, 3.0, 0.0 );

	zlaqr1( n, Hm.data, Hm.s1, Hm.s2, Hm.offset,
		new Complex128( 5.0, 0.0 ), new Complex128( 2.0, 0.0 ),
		v, 1, 0
	);

	vv = Array.from( reinterpret( v, 0 ) );
	assertArrayClose( vv, tc.v, 1e-13, 'v' );
});

test( 'zlaqr1: 2x2 with complex shifts', function t() {
	var tc = findCase( '2x2_complex_shifts' );
	var n = 2;
	var Hm = makeMatrix( n );
	var v = new Complex128Array( n );
	var vv;

	mset( Hm, n, 0, 0, 4.0, 1.0 );
	mset( Hm, n, 0, 1, 2.0, -1.0 );
	mset( Hm, n, 1, 0, 1.0, 0.5 );
	mset( Hm, n, 1, 1, 3.0, -0.5 );

	zlaqr1( n, Hm.data, Hm.s1, Hm.s2, Hm.offset,
		new Complex128( 3.0, 2.0 ), new Complex128( 1.0, -1.0 ),
		v, 1, 0
	);

	vv = Array.from( reinterpret( v, 0 ) );
	assertArrayClose( vv, tc.v, 1e-13, 'v' );
});

test( 'zlaqr1: 3x3 with real shifts', function t() {
	var tc = findCase( '3x3_real_shifts' );
	var n = 3;
	var Hm = makeMatrix( n );
	var v = new Complex128Array( n );
	var vv;

	mset( Hm, n, 0, 0, 5.0, 0.0 );
	mset( Hm, n, 0, 1, 2.0, 0.0 );
	mset( Hm, n, 0, 2, 1.0, 0.0 );
	mset( Hm, n, 1, 0, 1.0, 0.0 );
	mset( Hm, n, 1, 1, 4.0, 0.0 );
	mset( Hm, n, 1, 2, 3.0, 0.0 );
	mset( Hm, n, 2, 0, 0.0, 0.0 );
	mset( Hm, n, 2, 1, 2.0, 0.0 );
	mset( Hm, n, 2, 2, 3.0, 0.0 );

	zlaqr1( n, Hm.data, Hm.s1, Hm.s2, Hm.offset,
		new Complex128( 6.0, 0.0 ), new Complex128( 2.0, 0.0 ),
		v, 1, 0
	);

	vv = Array.from( reinterpret( v, 0 ) );
	assertArrayClose( vv, tc.v, 1e-13, 'v' );
});

test( 'zlaqr1: 3x3 with complex shifts', function t() {
	var tc = findCase( '3x3_complex_shifts' );
	var n = 3;
	var Hm = makeMatrix( n );
	var v = new Complex128Array( n );
	var vv;

	mset( Hm, n, 0, 0, 3.0, 1.0 );
	mset( Hm, n, 0, 1, 1.0, 0.5 );
	mset( Hm, n, 0, 2, 0.5, -0.5 );
	mset( Hm, n, 1, 0, 2.0, -1.0 );
	mset( Hm, n, 1, 1, 4.0, 0.0 );
	mset( Hm, n, 1, 2, 1.0, 1.0 );
	mset( Hm, n, 2, 0, 0.0, 0.0 );
	mset( Hm, n, 2, 1, 1.5, 0.5 );
	mset( Hm, n, 2, 2, 2.0, -1.0 );

	zlaqr1( n, Hm.data, Hm.s1, Hm.s2, Hm.offset,
		new Complex128( 2.0, 3.0 ), new Complex128( 1.0, -2.0 ),
		v, 1, 0
	);

	vv = Array.from( reinterpret( v, 0 ) );
	assertArrayClose( vv, tc.v, 1e-13, 'v' );
});

test( 'zlaqr1: 3x3 with conjugate pair shifts', function t() {
	var tc = findCase( '3x3_conjugate_shifts' );
	var n = 3;
	var Hm = makeMatrix( n );
	var v = new Complex128Array( n );
	var vv;

	mset( Hm, n, 0, 0, 6.0, 0.5 );
	mset( Hm, n, 0, 1, 3.0, -1.0 );
	mset( Hm, n, 0, 2, 1.0, 0.0 );
	mset( Hm, n, 1, 0, 1.0, 0.0 );
	mset( Hm, n, 1, 1, 5.0, -0.5 );
	mset( Hm, n, 1, 2, 2.0, 1.0 );
	mset( Hm, n, 2, 0, 0.0, 0.0 );
	mset( Hm, n, 2, 1, 0.5, 0.25 );
	mset( Hm, n, 2, 2, 4.0, 0.0 );

	zlaqr1( n, Hm.data, Hm.s1, Hm.s2, Hm.offset,
		new Complex128( 3.0, 1.5 ), new Complex128( 3.0, -1.5 ),
		v, 1, 0
	);

	vv = Array.from( reinterpret( v, 0 ) );
	assertArrayClose( vv, tc.v, 1e-13, 'v' );
});

test( 'zlaqr1: 2x2 with identical shifts', function t() {
	var tc = findCase( '2x2_identical_shifts' );
	var n = 2;
	var Hm = makeMatrix( n );
	var v = new Complex128Array( n );
	var vv;

	mset( Hm, n, 0, 0, 3.0, 1.0 );
	mset( Hm, n, 0, 1, 1.0, 0.0 );
	mset( Hm, n, 1, 0, 0.5, -0.5 );
	mset( Hm, n, 1, 1, 2.0, -1.0 );

	zlaqr1( n, Hm.data, Hm.s1, Hm.s2, Hm.offset,
		new Complex128( 3.0, 1.0 ), new Complex128( 3.0, 1.0 ),
		v, 1, 0
	);

	vv = Array.from( reinterpret( v, 0 ) );
	assertArrayClose( vv, tc.v, 1e-13, 'v' );
});
