'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var dswap = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dswap.jsonl' ), 'utf8' ).trim().split( '\n' );
var fixture = lines.map( function parse( line ) { return JSON.parse( line ); } );


// FUNCTIONS //

function findCase( name ) {
	return fixture.find( function find( t ) { return t.name === name; } );
}

function assertArrayClose( actual, expected, tol, msg ) {
	var i;
	assert.equal( actual.length, expected.length, msg + ': length mismatch' );
	for ( i = 0; i < expected.length; i++ ) {
		var relErr = Math.abs( actual[ i ] - expected[ i ] ) / Math.max( Math.abs( expected[ i ] ), 1.0 );
		assert.ok( relErr <= tol, msg + '[' + i + ']: expected ' + expected[ i ] + ', got ' + actual[ i ] );
	}
}


// TESTS //

test( 'dswap: main export is a function', function t() {
	assert.strictEqual( typeof dswap, 'function' );
});

test( 'dswap: basic (N=5, unit strides)', function t() {
	var tc = findCase( 'basic' );
	var x = new Float64Array( [ 1.0, 2.0, 3.0, 4.0, 5.0 ] );
	var y = new Float64Array( [ 6.0, 7.0, 8.0, 9.0, 10.0 ] );

	var out = dswap( 5, x, 1, 0, y, 1, 0 );

	assert.strictEqual( out, y, 'returns y' );
	assertArrayClose( Array.from( x ), tc.x, 1e-14, 'x' );
	assertArrayClose( Array.from( y ), tc.y, 1e-14, 'y' );
});

test( 'dswap: negative_stride (N=3, incx=2, incy=-1)', function t() {
	var tc = findCase( 'negative_stride' );
	var x = new Float64Array( [ 1.0, 0.0, 2.0, 0.0, 3.0 ] );
	var y = new Float64Array( [ 4.0, 5.0, 6.0 ] );

	// Fortran incy=-1 starts from y(3) (1-based), so offsetY=2 in 0-based
	dswap( 3, x, 2, 0, y, -1, 2 );

	assertArrayClose( Array.from( x ), tc.x, 1e-14, 'x' );
	assertArrayClose( Array.from( y ), tc.y, 1e-14, 'y' );
});

test( 'dswap: n_zero (N=0, quick return)', function t() {
	var tc = findCase( 'n_zero' );
	var x = new Float64Array( [ 1.0, 2.0, 3.0 ] );
	var y = new Float64Array( [ 4.0, 5.0, 6.0 ] );

	var out = dswap( 0, x, 1, 0, y, 1, 0 );

	assert.strictEqual( out, y, 'returns y' );
	assertArrayClose( Array.from( x ), tc.x, 1e-14, 'x' );
	assertArrayClose( Array.from( y ), tc.y, 1e-14, 'y' );
});

test( 'dswap: n_one (N=1)', function t() {
	var tc = findCase( 'n_one' );
	var x = new Float64Array( [ 42.0 ] );
	var y = new Float64Array( [ 99.0 ] );

	dswap( 1, x, 1, 0, y, 1, 0 );

	assertArrayClose( Array.from( x ), tc.x, 1e-14, 'x' );
	assertArrayClose( Array.from( y ), tc.y, 1e-14, 'y' );
});

test( 'dswap: N<0 returns y unchanged', function t() {
	var x = new Float64Array( [ 1.0, 2.0, 3.0 ] );
	var y = new Float64Array( [ 4.0, 5.0, 6.0 ] );

	var out = dswap( -1, x, 1, 0, y, 1, 0 );

	assert.strictEqual( out, y, 'returns y' );
	assert.deepStrictEqual( Array.from( x ), [ 1.0, 2.0, 3.0 ] );
	assert.deepStrictEqual( Array.from( y ), [ 4.0, 5.0, 6.0 ] );
});

test( 'dswap: offsetX and offsetY', function t() {
	var x = new Float64Array( [ 99.0, 1.0, 2.0, 3.0 ] );
	var y = new Float64Array( [ 99.0, 99.0, 4.0, 5.0, 6.0 ] );

	dswap( 3, x, 1, 1, y, 1, 2 );

	assert.deepStrictEqual( Array.from( x ), [ 99.0, 4.0, 5.0, 6.0 ] );
	assert.deepStrictEqual( Array.from( y ), [ 99.0, 99.0, 1.0, 2.0, 3.0 ] );
});

test( 'dswap: non-unit strides (strideX=2, strideY=3)', function t() {
	var x = new Float64Array( [ 1.0, 0.0, 2.0, 0.0, 3.0 ] );
	var y = new Float64Array( [ 10.0, 0.0, 0.0, 20.0, 0.0, 0.0, 30.0 ] );

	dswap( 3, x, 2, 0, y, 3, 0 );

	assert.strictEqual( x[ 0 ], 10.0 );
	assert.strictEqual( x[ 2 ], 20.0 );
	assert.strictEqual( x[ 4 ], 30.0 );
	assert.strictEqual( y[ 0 ], 1.0 );
	assert.strictEqual( y[ 3 ], 2.0 );
	assert.strictEqual( y[ 6 ], 3.0 );
	// Untouched elements remain
	assert.strictEqual( x[ 1 ], 0.0 );
	assert.strictEqual( x[ 3 ], 0.0 );
	assert.strictEqual( y[ 1 ], 0.0 );
	assert.strictEqual( y[ 2 ], 0.0 );
});
