

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
		assert.ok(
			Math.abs( actual[ i ] - expected[ i ] ) <= tol,
			msg + '[' + i + ']: expected ' + expected[ i ] + ', got ' + actual[ i ]
		);
	}
}


// TESTS //

test( 'dswap: main export is a function', function t() {
	assert.strictEqual( typeof dswap, 'function' );
});

test( 'dswap: basic swap (N=5, stride=1)', function t() {
	var tc = findCase( 'basic' );
	var x = new Float64Array( [ 1.0, 2.0, 3.0, 4.0, 5.0 ] );
	var y = new Float64Array( [ 6.0, 7.0, 8.0, 9.0, 10.0 ] );
	dswap( 5, x, 1, 0, y, 1, 0 );
	assertArrayClose( Array.from( x ), tc.x, 1e-14, 'x' );
	assertArrayClose( Array.from( y ), tc.y, 1e-14, 'y' );
});

test( 'dswap: negative stride (N=3, strideX=2, strideY=-1)', function t() {
	var tc = findCase( 'negative_stride' );
	// Fortran: x = [1,0,2,0,3] stride=2, y = [4,5,6] stride=-1
	// With negative stride in Fortran, y starts from the end
	// JS base.js: offsetY should point to last element for negative stride
	var x = new Float64Array( [ 1.0, 0.0, 2.0, 0.0, 3.0 ] );
	var y = new Float64Array( [ 4.0, 5.0, 6.0 ] );
	dswap( 3, x, 2, 0, y, -1, 2 );
	assertArrayClose( Array.from( x ), tc.x, 1e-14, 'x' );
	assertArrayClose( Array.from( y ), tc.y, 1e-14, 'y' );
});

test( 'dswap: N=0 quick return (vectors unchanged)', function t() {
	var tc = findCase( 'n_zero' );
	var x = new Float64Array( [ 1.0, 2.0, 3.0 ] );
	var y = new Float64Array( [ 4.0, 5.0, 6.0 ] );
	dswap( 0, x, 1, 0, y, 1, 0 );
	assertArrayClose( Array.from( x ), tc.x, 1e-14, 'x' );
	assertArrayClose( Array.from( y ), tc.y, 1e-14, 'y' );
});

test( 'dswap: N=1', function t() {
	var tc = findCase( 'n_one' );
	var x = new Float64Array( [ 42.0 ] );
	var y = new Float64Array( [ 99.0 ] );
	dswap( 1, x, 1, 0, y, 1, 0 );
	assertArrayClose( Array.from( x ), tc.x, 1e-14, 'x' );
	assertArrayClose( Array.from( y ), tc.y, 1e-14, 'y' );
});

test( 'dswap: returns y', function t() {
	var x = new Float64Array( [ 1.0, 2.0 ] );
	var y = new Float64Array( [ 3.0, 4.0 ] );
	var result = dswap( 2, x, 1, 0, y, 1, 0 );
	assert.strictEqual( result, y );
});

test( 'dswap: offset parameters work', function t() {
	var x = new Float64Array( [ 99.0, 1.0, 2.0 ] );
	var y = new Float64Array( [ 99.0, 3.0, 4.0 ] );
	dswap( 2, x, 1, 1, y, 1, 1 );
	assert.strictEqual( x[ 0 ], 99.0 ); // unchanged
	assert.strictEqual( x[ 1 ], 3.0 );
	assert.strictEqual( x[ 2 ], 4.0 );
	assert.strictEqual( y[ 0 ], 99.0 ); // unchanged
	assert.strictEqual( y[ 1 ], 1.0 );
	assert.strictEqual( y[ 2 ], 2.0 );
});

test( 'dswap: non-unit positive strides', function t() {
	// x = [1, _, 2, _, 3], strideX=2
	// y = [4, _, _, 5, _, _, 6], strideY=3
	var x = new Float64Array( [ 1.0, 0.0, 2.0, 0.0, 3.0 ] );
	var y = new Float64Array( [ 4.0, 0.0, 0.0, 5.0, 0.0, 0.0, 6.0 ] );
	dswap( 3, x, 2, 0, y, 3, 0 );
	assert.strictEqual( x[ 0 ], 4.0 );
	assert.strictEqual( x[ 2 ], 5.0 );
	assert.strictEqual( x[ 4 ], 6.0 );
	assert.strictEqual( y[ 0 ], 1.0 );
	assert.strictEqual( y[ 3 ], 2.0 );
	assert.strictEqual( y[ 6 ], 3.0 );
});

test( 'dswap: N negative returns y unchanged', function t() {
	var x = new Float64Array( [ 1.0, 2.0 ] );
	var y = new Float64Array( [ 3.0, 4.0 ] );
	dswap( -1, x, 1, 0, y, 1, 0 );
	assert.strictEqual( x[ 0 ], 1.0 );
	assert.strictEqual( y[ 0 ], 3.0 );
});
