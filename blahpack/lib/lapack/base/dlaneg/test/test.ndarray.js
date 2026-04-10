/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var readFileSync = require( 'fs' ).readFileSync; // eslint-disable-line node/no-sync
var path = require( 'path' );
var assert = require( 'node:assert/strict' );
var format = require( '@stdlib/string/format' );
var Float64Array = require( '@stdlib/array/float64' );
var base = require( './../lib/base.js' );
var ndarrayFn = require( './../lib/ndarray.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' ); // eslint-disable-line max-len
var fixture = readFileSync( path.join( fixtureDir, 'dlaneg.jsonl' ), 'utf8' ) // eslint-disable-line node/no-sync
	.trim()
	.split( '\n' )
	.map( function parse( line ) {
		return JSON.parse( line );
	});


// FUNCTIONS //

/**
* Constructs the inputs for a named test case.
*
* @private
* @param {string} name - test case name
* @throws {Error} if the case name is unknown
* @returns {Object} test case inputs
*/
function buildCase( name ) {
	var lld;
	var d;
	var i;
	switch ( name ) {
	case 'n1_sigma_below':
		return { 'N': 1, 'd': new Float64Array( [ 2.0 ] ), 'lld': new Float64Array( 0 ), 'sigma': 0.0, 'pivmin': 1e-30, 'r': 1 };
	case 'n1_sigma_above':
		return { 'N': 1, 'd': new Float64Array( [ 2.0 ] ), 'lld': new Float64Array( 0 ), 'sigma': 5.0, 'pivmin': 1e-30, 'r': 1 };
	case 'n5_sigma_zero_r3':
		return { 'N': 5, 'd': new Float64Array( [ 4.0, 3.0, 2.0, 1.0, 5.0 ] ), 'lld': new Float64Array( [ 0.5, 0.5, 0.5, 0.5 ] ), 'sigma': 0.0, 'pivmin': 1e-30, 'r': 3 };
	case 'n5_sigma_zero_r5':
		return { 'N': 5, 'd': new Float64Array( [ 4.0, 3.0, 2.0, 1.0, 5.0 ] ), 'lld': new Float64Array( [ 0.5, 0.5, 0.5, 0.5 ] ), 'sigma': 0.0, 'pivmin': 1e-30, 'r': 5 };
	case 'n5_sigma_zero_r1':
		return { 'N': 5, 'd': new Float64Array( [ 4.0, 3.0, 2.0, 1.0, 5.0 ] ), 'lld': new Float64Array( [ 0.5, 0.5, 0.5, 0.5 ] ), 'sigma': 0.0, 'pivmin': 1e-30, 'r': 1 };
	case 'n5_sigma_large_r3':
		return { 'N': 5, 'd': new Float64Array( [ 4.0, 3.0, 2.0, 1.0, 5.0 ] ), 'lld': new Float64Array( [ 0.5, 0.5, 0.5, 0.5 ] ), 'sigma': 10.0, 'pivmin': 1e-30, 'r': 3 };
	case 'n4_mixed_r2':
		return { 'N': 4, 'd': new Float64Array( [ -1.0, 2.0, -3.0, 4.0 ] ), 'lld': new Float64Array( [ 0.25, 0.25, 0.25 ] ), 'sigma': 0.0, 'pivmin': 1e-30, 'r': 2 };
	case 'n4_mixed_neg_sigma':
		return { 'N': 4, 'd': new Float64Array( [ -1.0, 2.0, -3.0, 4.0 ] ), 'lld': new Float64Array( [ 0.25, 0.25, 0.25 ] ), 'sigma': -1.0, 'pivmin': 1e-30, 'r': 2 };
	case 'n150_sigma_zero':
	case 'n150_sigma_50':
	case 'n150_r_n':
	case 'n150_r_1':
		d = new Float64Array( 150 );
		lld = new Float64Array( 149 );
		for ( i = 0; i < 150; i++ ) {
			d[ i ] = i + 1;
		}
		for ( i = 0; i < 149; i++ ) {
			lld[ i ] = 0.1;
		}
		if ( name === 'n150_sigma_zero' ) {
			return { 'N': 150, 'd': d, 'lld': lld, 'sigma': 0.0, 'pivmin': 1e-30, 'r': 75 };
		}
		if ( name === 'n150_sigma_50' ) {
			return { 'N': 150, 'd': d, 'lld': lld, 'sigma': 50.5, 'pivmin': 1e-30, 'r': 75 };
		}
		if ( name === 'n150_r_n' ) {
			return { 'N': 150, 'd': d, 'lld': lld, 'sigma': 0.0, 'pivmin': 1e-30, 'r': 150 };
		}
		return { 'N': 150, 'd': d, 'lld': lld, 'sigma': 0.0, 'pivmin': 1e-30, 'r': 1 };
	case 'n2_r1':
		return { 'N': 2, 'd': new Float64Array( [ 1.0, 4.0 ] ), 'lld': new Float64Array( [ 1.0 ] ), 'sigma': 0.5, 'pivmin': 1e-30, 'r': 1 };
	case 'n2_r2':
		return { 'N': 2, 'd': new Float64Array( [ 1.0, 4.0 ] ), 'lld': new Float64Array( [ 1.0 ] ), 'sigma': 0.5, 'pivmin': 1e-30, 'r': 2 };
	default:
		throw new Error( format( 'unknown case: `%s`.', name ) );
	}
}


// TESTS //

test( 'base is a function', function t() {
	assert.strictEqual( typeof base, 'function', 'is a function' );
});

test( 'ndarray is a function', function t() {
	assert.strictEqual( typeof ndarrayFn, 'function', 'is a function' );
});

test( 'ndarray has expected arity', function t() {
	assert.strictEqual( ndarrayFn.length, 10, 'has expected arity' );
});

// Every fixture case is executed via the ndarray entry point with exact
// integer comparison against the Fortran reference `negcnt`.
fixture.forEach( function each( tc ) {
	test( 'ndarray: ' + tc.name, function t() {
		var inputs = buildCase( tc.name );
		var result = ndarrayFn(
			inputs.N,
			inputs.d, 1, 0,
			inputs.lld, 1, 0,
			inputs.sigma, inputs.pivmin, inputs.r
		);
		assert.equal( result, tc.negcnt, format( '%s: expected %d, got %d', tc.name, tc.negcnt, result ) ); // eslint-disable-line max-len
	});
});

test( 'ndarray: non-unit positive strides with sentinel padding', function t() {
	var SENTINEL;
	var lbase;
	var base0;
	var lld;
	var dArr;
	var r1;
	var r2;
	var N;
	var i;

	SENTINEL = -9.9e99;
	base0 = [ 4.0, 3.0, 2.0, 1.0, 5.0 ];
	lbase = [ 0.5, 0.5, 0.5, 0.5 ];
	N = 5;

	// Pre-fill with sentinels so the routine must skip the gaps correctly.
	dArr = new Float64Array( N * 2 );
	lld = new Float64Array( ( N - 1 ) * 3 );
	for ( i = 0; i < dArr.length; i++ ) {
		dArr[ i ] = SENTINEL;
	}
	for ( i = 0; i < lld.length; i++ ) {
		lld[ i ] = SENTINEL;
	}
	for ( i = 0; i < N; i++ ) {
		dArr[ i * 2 ] = base0[ i ];
	}
	for ( i = 0; i < N - 1; i++ ) {
		lld[ i * 3 ] = lbase[ i ];
	}

	r1 = ndarrayFn( N, dArr, 2, 0, lld, 3, 0, 10.0, 1e-30, 3 );
	r2 = base( N, new Float64Array( base0 ), 1, 0, new Float64Array( lbase ), 1, 0, 10.0, 1e-30, 3 ); // eslint-disable-line max-len
	assert.equal( r1, r2, 'strided ndarray result matches unit-stride base result' ); // eslint-disable-line max-len
	assert.equal( r1, 5, 'expected Sturm count of 5 for sigma=10 on this matrix' ); // eslint-disable-line max-len

	// Verify sentinels untouched (dlaneg is read-only, but assert it).
	for ( i = 0; i < N - 1; i++ ) {
		assert.equal( dArr[ ( i * 2 ) + 1 ], SENTINEL, 'd sentinel preserved' );
	}
	for ( i = 0; i < ( N - 1 ); i++ ) {
		assert.equal( lld[ ( i * 3 ) + 1 ], SENTINEL, 'lld sentinel +1 preserved' ); // eslint-disable-line max-len
		assert.equal( lld[ ( i * 3 ) + 2 ], SENTINEL, 'lld sentinel +2 preserved' ); // eslint-disable-line max-len
	}
});

test( 'ndarray: non-zero offsets with sentinel padding', function t() {
	var SENTINEL;
	var lld;
	var dArr;
	var r;
	var i;

	SENTINEL = 1.2345e77;
	dArr = new Float64Array( [ SENTINEL, SENTINEL, 4.0, 3.0, 2.0, 1.0, 5.0 ] );
	lld = new Float64Array( [ SENTINEL, 0.5, 0.5, 0.5, 0.5 ] );

	r = ndarrayFn( 5, dArr, 1, 2, lld, 1, 1, 10.0, 1e-30, 3 );
	assert.equal( r, 5 );
	assert.equal( dArr[ 0 ], SENTINEL, 'd[0] sentinel preserved' );
	assert.equal( dArr[ 1 ], SENTINEL, 'd[1] sentinel preserved' );
	assert.equal( lld[ 0 ], SENTINEL, 'lld[0] sentinel preserved' );
});

test( 'ndarray: strided + offset combined', function t() {
	var base0;
	var lbase;
	var lld;
	var dArr;
	var N;
	var r;
	var i;

	base0 = [ 4.0, 3.0, 2.0, 1.0, 5.0 ];
	lbase = [ 0.5, 0.5, 0.5, 0.5 ];
	N = 5;

	// d: 3-element prefix padding, stride 2
	dArr = new Float64Array( 3 + ( N * 2 ) );
	for ( i = 0; i < dArr.length; i++ ) {
		dArr[ i ] = -7.0;
	}
	for ( i = 0; i < N; i++ ) {
		dArr[ 3 + ( i * 2 ) ] = base0[ i ];
	}

	// lld: 1-element prefix padding, stride 2
	lld = new Float64Array( 1 + ( ( N - 1 ) * 2 ) );
	for ( i = 0; i < lld.length; i++ ) {
		lld[ i ] = -7.0;
	}
	for ( i = 0; i < N - 1; i++ ) {
		lld[ 1 + ( i * 2 ) ] = lbase[ i ];
	}

	r = ndarrayFn( N, dArr, 2, 3, lld, 2, 1, 10.0, 1e-30, 3 );
	assert.equal( r, 5, 'strided+offset ndarray result matches expected Sturm count' ); // eslint-disable-line max-len
});
