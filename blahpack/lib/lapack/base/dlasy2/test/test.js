/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';


// MODULES //

var test = require( 'node:test' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dlasy2 = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' ); // eslint-disable-line max-len
var lines = readFileSync( path.join( fixtureDir, 'dlasy2.jsonl' ), 'utf8' ).trim().split( '\n' ); // eslint-disable-line node/no-sync
var fixture = lines.map( function parse( line ) {
	return JSON.parse( line );
} );


// FUNCTIONS //

/**
* Returns a test case from the fixture data.
*
* @private
* @param {string} name - test case name
* @returns {*} result
*/
function findCase( name ) {
	return fixture.find( function find( t ) { return t.name === name;
	} );
}

/**
* Asserts that two numbers are approximately equal.
*
* @private
* @param {*} actual - actual value
* @param {*} expected - expected value
* @param {number} tol - tolerance
* @param {string} msg - assertion message
*/
function assertClose( actual, expected, tol, msg ) {
	var relErr = Math.abs( actual - expected ) / Math.max( Math.abs( expected ), 1.0 ); // eslint-disable-line max-len
	assert.ok( relErr <= tol, msg + ': expected ' + expected + ', got ' + actual );
}

/**
* Asserts that two arrays are element-wise approximately equal.
*
* @private
* @param {*} actual - actual value
* @param {*} expected - expected value
* @param {number} tol - tolerance
* @param {string} msg - assertion message
*/
function assertArrayClose( actual, expected, tol, msg ) {
	var i;
	assert.equal( actual.length, expected.length, msg + ': length mismatch' );
	for ( i = 0; i < expected.length; i++ ) {
		assertClose( actual[ i ], expected[ i ], tol, msg + '[' + i + ']' );
	}
}

/**
* Converts a typed array to a plain array.
*
* @private
* @param {TypedArray} arr - input array
* @returns {Array} output array
*/
function toArray( arr ) {
	var out = [];
	var i;
	for ( i = 0; i < arr.length; i++ ) {
		out.push( arr[ i ] );
	}
	return out;
}


// TESTS //

test( 'dlasy2: n1=1 n2=1 basic', function t() {
	var scale;
	var xnorm;
	var info;
	var tc;
	var TL;
	var TR;
	var B;
	var X;

	tc = findCase( 'n1=1 n2=1 basic' );
	TL = new Float64Array([ 3.0, 0, 0, 0 ]);
	TR = new Float64Array([ 2.0, 0, 0, 0 ]);
	B = new Float64Array([ 10.0, 0, 0, 0 ]);
	X = new Float64Array( 4 );
	scale = new Float64Array( 1 );
	xnorm = new Float64Array( 1 );
	info = dlasy2( false, false, 1, 1, 1, TL, 1, 2, 0, TR, 1, 2, 0, B, 1, 2, 0, scale, X, 1, 2, 0, xnorm ); // eslint-disable-line max-len
	assert.strictEqual( info, tc.info, 'info' );
	assertClose( scale[ 0 ], tc.scale, 1e-14, 'scale' );
	assertClose( xnorm[ 0 ], tc.xnorm, 1e-14, 'xnorm' );
	assertArrayClose( toArray( X ), tc.X, 1e-14, 'X' );
});

test( 'dlasy2: n1=1 n2=2', function t() {
	var scale;
	var xnorm;
	var info;
	var tc;
	var TL;
	var TR;
	var B;
	var X;

	tc = findCase( 'n1=1 n2=2' );
	TL = new Float64Array([ 2.0, 0, 0, 0 ]);
	TR = new Float64Array([ 1.0, -0.5, 0.5, 1.0 ]);
	B = new Float64Array([ 3.0, 0, 4.0, 0 ]);
	X = new Float64Array( 4 );
	scale = new Float64Array( 1 );
	xnorm = new Float64Array( 1 );
	info = dlasy2( false, false, 1, 1, 2, TL, 1, 2, 0, TR, 1, 2, 0, B, 1, 2, 0, scale, X, 1, 2, 0, xnorm ); // eslint-disable-line max-len
	assert.strictEqual( info, tc.info, 'info' );
	assertClose( scale[ 0 ], tc.scale, 1e-14, 'scale' );
	assertClose( xnorm[ 0 ], tc.xnorm, 1e-14, 'xnorm' );
	assertArrayClose( toArray( X ), tc.X, 1e-14, 'X' );
});

test( 'dlasy2: n1=2 n2=1', function t() {
	var scale;
	var xnorm;
	var info;
	var tc;
	var TL;
	var TR;
	var B;
	var X;

	tc = findCase( 'n1=2 n2=1' );
	TL = new Float64Array([ 2.0, -0.5, 0.5, 2.0 ]);
	TR = new Float64Array([ 1.0, 0, 0, 0 ]);
	B = new Float64Array([ 3.0, 4.0, 0, 0 ]);
	X = new Float64Array( 4 );
	scale = new Float64Array( 1 );
	xnorm = new Float64Array( 1 );
	info = dlasy2( false, false, 1, 2, 1, TL, 1, 2, 0, TR, 1, 2, 0, B, 1, 2, 0, scale, X, 1, 2, 0, xnorm ); // eslint-disable-line max-len
	assert.strictEqual( info, tc.info, 'info' );
	assertClose( scale[ 0 ], tc.scale, 1e-14, 'scale' );
	assertClose( xnorm[ 0 ], tc.xnorm, 1e-14, 'xnorm' );
	assertArrayClose( toArray( X ), tc.X, 1e-14, 'X' );
});

test( 'dlasy2: n1=2 n2=2', function t() {
	var scale;
	var xnorm;
	var info;
	var tc;
	var TL;
	var TR;
	var B;
	var X;

	tc = findCase( 'n1=2 n2=2' );
	TL = new Float64Array([ 1.0, -0.5, 0.5, 1.0 ]);
	TR = new Float64Array([ 2.0, -0.3, 0.3, 2.0 ]);
	B = new Float64Array([ 1.0, 3.0, 2.0, 4.0 ]);
	X = new Float64Array( 4 );
	scale = new Float64Array( 1 );
	xnorm = new Float64Array( 1 );
	info = dlasy2( false, false, 1, 2, 2, TL, 1, 2, 0, TR, 1, 2, 0, B, 1, 2, 0, scale, X, 1, 2, 0, xnorm ); // eslint-disable-line max-len
	assert.strictEqual( info, tc.info, 'info' );
	assertClose( scale[ 0 ], tc.scale, 1e-14, 'scale' );
	assertClose( xnorm[ 0 ], tc.xnorm, 1e-14, 'xnorm' );
	assertArrayClose( toArray( X ), tc.X, 1e-14, 'X' );
});

test( 'dlasy2: quick return for n1=0', function t() {
	var scale;
	var xnorm;
	var info;
	var X;

	X = new Float64Array( 4 );
	scale = new Float64Array( 1 );
	xnorm = new Float64Array( 1 );
	info = dlasy2( false, false, 1, 0, 1, new Float64Array(4), 1, 2, 0, new Float64Array(4), 1, 2, 0, new Float64Array(4), 1, 2, 0, scale, X, 1, 2, 0, xnorm ); // eslint-disable-line max-len
	assert.strictEqual( info, 0 );
});

test( 'dlasy2: n1=1 n2=1 ltranl=T ltranr=T isgn=-1', function t() {
	var scale;
	var xnorm;
	var info;
	var tc;
	var TL;
	var TR;
	var B;
	var X;

	tc = findCase( 'n1=1 n2=1 ltranl=T ltranr=T isgn=-1' );
	TL = new Float64Array([ 3.0, 0, 0, 0 ]);
	TR = new Float64Array([ 2.0, 0, 0, 0 ]);
	B = new Float64Array([ 10.0, 0, 0, 0 ]);
	X = new Float64Array( 4 );
	scale = new Float64Array( 1 );
	xnorm = new Float64Array( 1 );
	info = dlasy2( true, true, -1, 1, 1, TL, 1, 2, 0, TR, 1, 2, 0, B, 1, 2, 0, scale, X, 1, 2, 0, xnorm ); // eslint-disable-line max-len
	assert.strictEqual( info, tc.info, 'info' );
	assertClose( scale[ 0 ], tc.scale, 1e-14, 'scale' );
	assertClose( xnorm[ 0 ], tc.xnorm, 1e-14, 'xnorm' );
	assertArrayClose( toArray( X ), tc.X, 1e-14, 'X' );
});

test( 'dlasy2: n1=1 n2=2 ltranr=T', function t() {
	var scale;
	var xnorm;
	var info;
	var tc;
	var TL;
	var TR;
	var B;
	var X;

	tc = findCase( 'n1=1 n2=2 ltranr=T' );
	TL = new Float64Array([ 2.0, 0, 0, 0 ]);
	TR = new Float64Array([ 1.0, -0.5, 0.5, 1.0 ]);
	B = new Float64Array([ 3.0, 0, 4.0, 0 ]);
	X = new Float64Array( 4 );
	scale = new Float64Array( 1 );
	xnorm = new Float64Array( 1 );
	info = dlasy2( false, true, 1, 1, 2, TL, 1, 2, 0, TR, 1, 2, 0, B, 1, 2, 0, scale, X, 1, 2, 0, xnorm ); // eslint-disable-line max-len
	assert.strictEqual( info, tc.info, 'info' );
	assertClose( scale[ 0 ], tc.scale, 1e-14, 'scale' );
	assertClose( xnorm[ 0 ], tc.xnorm, 1e-14, 'xnorm' );
	assertArrayClose( toArray( X ), tc.X, 1e-14, 'X' );
});

test( 'dlasy2: n1=2 n2=1 ltranl=T', function t() {
	var scale;
	var xnorm;
	var info;
	var tc;
	var TL;
	var TR;
	var B;
	var X;

	tc = findCase( 'n1=2 n2=1 ltranl=T' );
	TL = new Float64Array([ 2.0, -0.5, 0.5, 2.0 ]);
	TR = new Float64Array([ 1.0, 0, 0, 0 ]);
	B = new Float64Array([ 3.0, 4.0, 0, 0 ]);
	X = new Float64Array( 4 );
	scale = new Float64Array( 1 );
	xnorm = new Float64Array( 1 );
	info = dlasy2( true, false, 1, 2, 1, TL, 1, 2, 0, TR, 1, 2, 0, B, 1, 2, 0, scale, X, 1, 2, 0, xnorm ); // eslint-disable-line max-len
	assert.strictEqual( info, tc.info, 'info' );
	assertClose( scale[ 0 ], tc.scale, 1e-14, 'scale' );
	assertClose( xnorm[ 0 ], tc.xnorm, 1e-14, 'xnorm' );
	assertArrayClose( toArray( X ), tc.X, 1e-14, 'X' );
});

test( 'dlasy2: n1=2 n2=2 ltranl=T ltranr=T', function t() {
	var scale;
	var xnorm;
	var info;
	var tc;
	var TL;
	var TR;
	var B;
	var X;

	tc = findCase( 'n1=2 n2=2 ltranl=T ltranr=T' );
	TL = new Float64Array([ 1.0, -0.5, 0.5, 1.0 ]);
	TR = new Float64Array([ 2.0, -0.3, 0.3, 2.0 ]);
	B = new Float64Array([ 1.0, 3.0, 2.0, 4.0 ]);
	X = new Float64Array( 4 );
	scale = new Float64Array( 1 );
	xnorm = new Float64Array( 1 );
	info = dlasy2( true, true, 1, 2, 2, TL, 1, 2, 0, TR, 1, 2, 0, B, 1, 2, 0, scale, X, 1, 2, 0, xnorm ); // eslint-disable-line max-len
	assert.strictEqual( info, tc.info, 'info' );
	assertClose( scale[ 0 ], tc.scale, 1e-14, 'scale' );
	assertClose( xnorm[ 0 ], tc.xnorm, 1e-14, 'xnorm' );
	assertArrayClose( toArray( X ), tc.X, 1e-14, 'X' );
});

test( 'dlasy2: n1=2 n2=2 ltranl=T isgn=-1', function t() {
	var scale;
	var xnorm;
	var info;
	var tc;
	var TL;
	var TR;
	var B;
	var X;

	tc = findCase( 'n1=2 n2=2 ltranl=T isgn=-1' );
	TL = new Float64Array([ 1.0, -0.5, 0.5, 1.0 ]);
	TR = new Float64Array([ 2.0, -0.3, 0.3, 2.0 ]);
	B = new Float64Array([ 5.0, 7.0, 6.0, 8.0 ]);
	X = new Float64Array( 4 );
	scale = new Float64Array( 1 );
	xnorm = new Float64Array( 1 );
	info = dlasy2( true, false, -1, 2, 2, TL, 1, 2, 0, TR, 1, 2, 0, B, 1, 2, 0, scale, X, 1, 2, 0, xnorm ); // eslint-disable-line max-len
	assert.strictEqual( info, tc.info, 'info' );
	assertClose( scale[ 0 ], tc.scale, 1e-14, 'scale' );
	assertClose( xnorm[ 0 ], tc.xnorm, 1e-14, 'xnorm' );
	assertArrayClose( toArray( X ), tc.X, 1e-14, 'X' );
});

test( 'dlasy2: n1=2 n2=2 ltranr=T isgn=-1', function t() {
	var scale;
	var xnorm;
	var info;
	var tc;
	var TL;
	var TR;
	var B;
	var X;

	tc = findCase( 'n1=2 n2=2 ltranr=T isgn=-1' );
	TL = new Float64Array([ 1.0, -0.5, 0.5, 1.0 ]);
	TR = new Float64Array([ 2.0, -0.3, 0.3, 2.0 ]);
	B = new Float64Array([ 5.0, 7.0, 6.0, 8.0 ]);
	X = new Float64Array( 4 );
	scale = new Float64Array( 1 );
	xnorm = new Float64Array( 1 );
	info = dlasy2( false, true, -1, 2, 2, TL, 1, 2, 0, TR, 1, 2, 0, B, 1, 2, 0, scale, X, 1, 2, 0, xnorm ); // eslint-disable-line max-len
	assert.strictEqual( info, tc.info, 'info' );
	assertClose( scale[ 0 ], tc.scale, 1e-14, 'scale' );
	assertClose( xnorm[ 0 ], tc.xnorm, 1e-14, 'xnorm' );
	assertArrayClose( toArray( X ), tc.X, 1e-14, 'X' );
});

test( 'dlasy2: n1=1 n2=1 isgn=-1', function t() {
	var scale;
	var xnorm;
	var info;
	var tc;
	var TL;
	var TR;
	var B;
	var X;

	tc = findCase( 'n1=1 n2=1 isgn=-1' );
	TL = new Float64Array([ 3.0, 0, 0, 0 ]);
	TR = new Float64Array([ 2.0, 0, 0, 0 ]);
	B = new Float64Array([ 10.0, 0, 0, 0 ]);
	X = new Float64Array( 4 );
	scale = new Float64Array( 1 );
	xnorm = new Float64Array( 1 );
	info = dlasy2( false, false, -1, 1, 1, TL, 1, 2, 0, TR, 1, 2, 0, B, 1, 2, 0, scale, X, 1, 2, 0, xnorm ); // eslint-disable-line max-len
	assert.strictEqual( info, tc.info, 'info' );
	assertClose( scale[ 0 ], tc.scale, 1e-14, 'scale' );
	assertClose( xnorm[ 0 ], tc.xnorm, 1e-14, 'xnorm' );
	assertArrayClose( toArray( X ), tc.X, 1e-14, 'X' );
});

test( 'dlasy2: n1=1 n2=2 isgn=-1', function t() {
	var scale;
	var xnorm;
	var info;
	var tc;
	var TL;
	var TR;
	var B;
	var X;

	tc = findCase( 'n1=1 n2=2 isgn=-1' );
	TL = new Float64Array([ 2.0, 0, 0, 0 ]);
	TR = new Float64Array([ 1.0, -0.5, 0.5, 1.0 ]);
	B = new Float64Array([ 3.0, 0, 4.0, 0 ]);
	X = new Float64Array( 4 );
	scale = new Float64Array( 1 );
	xnorm = new Float64Array( 1 );
	info = dlasy2( false, false, -1, 1, 2, TL, 1, 2, 0, TR, 1, 2, 0, B, 1, 2, 0, scale, X, 1, 2, 0, xnorm ); // eslint-disable-line max-len
	assert.strictEqual( info, tc.info, 'info' );
	assertClose( scale[ 0 ], tc.scale, 1e-14, 'scale' );
	assertClose( xnorm[ 0 ], tc.xnorm, 1e-14, 'xnorm' );
	assertArrayClose( toArray( X ), tc.X, 1e-14, 'X' );
});

test( 'dlasy2: n1=2 n2=1 isgn=-1', function t() {
	var scale;
	var xnorm;
	var info;
	var tc;
	var TL;
	var TR;
	var B;
	var X;

	tc = findCase( 'n1=2 n2=1 isgn=-1' );
	TL = new Float64Array([ 2.0, -0.5, 0.5, 2.0 ]);
	TR = new Float64Array([ 1.0, 0, 0, 0 ]);
	B = new Float64Array([ 3.0, 4.0, 0, 0 ]);
	X = new Float64Array( 4 );
	scale = new Float64Array( 1 );
	xnorm = new Float64Array( 1 );
	info = dlasy2( false, false, -1, 2, 1, TL, 1, 2, 0, TR, 1, 2, 0, B, 1, 2, 0, scale, X, 1, 2, 0, xnorm ); // eslint-disable-line max-len
	assert.strictEqual( info, tc.info, 'info' );
	assertClose( scale[ 0 ], tc.scale, 1e-14, 'scale' );
	assertClose( xnorm[ 0 ], tc.xnorm, 1e-14, 'xnorm' );
	assertArrayClose( toArray( X ), tc.X, 1e-14, 'X' );
});

test( 'dlasy2: n1=1 n2=1 near-singular', function t() {
	var scale;
	var xnorm;
	var info;
	var tc;
	var TL;
	var TR;
	var B;
	var X;

	tc = findCase( 'n1=1 n2=1 near-singular' );
	TL = new Float64Array([ 1.0e-300, 0, 0, 0 ]);
	TR = new Float64Array([ -1.0e-300, 0, 0, 0 ]);
	B = new Float64Array([ 1.0, 0, 0, 0 ]);
	X = new Float64Array( 4 );
	scale = new Float64Array( 1 );
	xnorm = new Float64Array( 1 );
	info = dlasy2( false, false, 1, 1, 1, TL, 1, 2, 0, TR, 1, 2, 0, B, 1, 2, 0, scale, X, 1, 2, 0, xnorm ); // eslint-disable-line max-len
	assert.strictEqual( info, tc.info, 'info' );
	assertClose( scale[ 0 ], tc.scale, 1e-14, 'scale' );
	assertClose( xnorm[ 0 ], tc.xnorm, 1e-10, 'xnorm' );
	assertArrayClose( toArray( X ), tc.X, 1e-10, 'X' );
});
