'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var Complex128 = require( '@stdlib/complex/float64/ctor' );
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var zhpr2 = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'zhpr2.jsonl' ), 'utf8' ).trim().split( '\n' );
var fixture = lines.map( function parse( line ) { return JSON.parse( line ); } );


// FUNCTIONS //

function findCase( name ) {
	return fixture.find( function find( t ) { return t.name === name; } );
}

function assertArrayClose( actual, expected, tol, msg ) {
	var relErr;
	var i;
	assert.equal( actual.length, expected.length, msg + ': length mismatch' );
	for ( i = 0; i < expected.length; i += 1 ) {
		relErr = Math.abs( actual[ i ] - expected[ i ] ) / Math.max( Math.abs( expected[ i ] ), 1.0 );
		assert.ok( relErr <= tol, msg + '[' + i + ']: expected ' + expected[ i ] + ', got ' + actual[ i ] );
	}
}


// TESTS //

test( 'zhpr2 is a function', function t() {
	assert.strictEqual( typeof zhpr2, 'function' );
});

test( 'zhpr2: upper triangle, N=3, alpha=(1,0)', function t() {
	var alpha;
	var tc;
	var ap;
	var rv;
	var x;
	var y;

	tc = findCase( 'upper_basic' );
	ap = new Complex128Array( [ 2.0, 0.0, 1.0, 1.0, 4.0, 0.0, 3.0, -2.0, 2.0, 1.0, 5.0, 0.0 ] );
	x = new Complex128Array( [ 1.0, 0.5, 2.0, -1.0, 3.0, 1.0 ] );
	y = new Complex128Array( [ 0.5, 1.0, 1.5, -0.5, 2.5, 0.0 ] );
	alpha = new Complex128( 1.0, 0.0 );

	zhpr2( 'upper', 3, alpha, x, 1, 0, y, 1, 0, ap, 1, 0 );

	rv = reinterpret( ap, 0 );
	assertArrayClose( rv, tc.AP, 1e-14, 'AP' );
});

test( 'zhpr2: lower triangle, N=3, alpha=(1,0)', function t() {
	var alpha;
	var tc;
	var ap;
	var rv;
	var x;
	var y;

	tc = findCase( 'lower_basic' );
	ap = new Complex128Array( [ 2.0, 0.0, 1.0, -1.0, 3.0, 2.0, 4.0, 0.0, 2.0, -1.0, 5.0, 0.0 ] );
	x = new Complex128Array( [ 1.0, 0.5, 2.0, -1.0, 3.0, 1.0 ] );
	y = new Complex128Array( [ 0.5, 1.0, 1.5, -0.5, 2.5, 0.0 ] );
	alpha = new Complex128( 1.0, 0.0 );

	zhpr2( 'lower', 3, alpha, x, 1, 0, y, 1, 0, ap, 1, 0 );

	rv = reinterpret( ap, 0 );
	assertArrayClose( rv, tc.AP, 1e-14, 'AP' );
});

test( 'zhpr2: complex alpha=(2,1)', function t() {
	var alpha;
	var tc;
	var ap;
	var rv;
	var x;
	var y;

	tc = findCase( 'complex_alpha' );
	ap = new Complex128Array( [ 2.0, 0.0, 1.0, 1.0, 4.0, 0.0, 3.0, -2.0, 2.0, 1.0, 5.0, 0.0 ] );
	x = new Complex128Array( [ 1.0, 0.5, 2.0, -1.0, 3.0, 1.0 ] );
	y = new Complex128Array( [ 0.5, 1.0, 1.5, -0.5, 2.5, 0.0 ] );
	alpha = new Complex128( 2.0, 1.0 );

	zhpr2( 'upper', 3, alpha, x, 1, 0, y, 1, 0, ap, 1, 0 );

	rv = reinterpret( ap, 0 );
	assertArrayClose( rv, tc.AP, 1e-14, 'AP' );
});

test( 'zhpr2: alpha=0 is a no-op', function t() {
	var expected;
	var alpha;
	var ap;
	var rv;
	var x;
	var y;

	expected = [ 2.0, 0.0, 1.0, 1.0, 4.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 ];
	ap = new Complex128Array( [ 2.0, 0.0, 1.0, 1.0, 4.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 ] );
	x = new Complex128Array( [ 1.0, 0.5, 2.0, -1.0, 3.0, 1.0 ] );
	y = new Complex128Array( [ 0.5, 1.0, 1.5, -0.5, 2.5, 0.0 ] );
	alpha = new Complex128( 0.0, 0.0 );

	zhpr2( 'upper', 3, alpha, x, 1, 0, y, 1, 0, ap, 1, 0 );

	rv = reinterpret( ap, 0 );
	assertArrayClose( rv, expected, 1e-14, 'AP' );
});

test( 'zhpr2: N=0 quick return', function t() {
	var alpha;
	var tc;
	var ap;
	var rv;
	var x;
	var y;

	tc = findCase( 'n_zero' );
	ap = new Complex128Array( [ 99.0, 0.0 ] );
	x = new Complex128Array( [ 1.0, 0.5 ] );
	y = new Complex128Array( [ 0.5, 1.0 ] );
	alpha = new Complex128( 1.0, 0.0 );

	zhpr2( 'upper', 0, alpha, x, 1, 0, y, 1, 0, ap, 1, 0 );

	rv = reinterpret( ap, 0 );
	assertArrayClose( rv, tc.AP, 1e-14, 'AP' );
});

test( 'zhpr2: N=1 scalar case', function t() {
	var alpha;
	var tc;
	var ap;
	var rv;
	var x;
	var y;

	tc = findCase( 'scalar' );
	ap = new Complex128Array( [ 3.0, 0.0 ] );
	x = new Complex128Array( [ 2.0, 1.0 ] );
	y = new Complex128Array( [ 1.0, -0.5 ] );
	alpha = new Complex128( 1.0, 0.5 );

	zhpr2( 'upper', 1, alpha, x, 1, 0, y, 1, 0, ap, 1, 0 );

	rv = reinterpret( ap, 0 );
	assertArrayClose( rv, tc.AP, 1e-14, 'AP' );
});

test( 'zhpr2: non-unit stride (incx=2, incy=2)', function t() {
	var alpha;
	var tc;
	var ap;
	var rv;
	var x;
	var y;

	tc = findCase( 'stride_2' );
	ap = new Complex128Array( [ 2.0, 0.0, 1.0, 1.0, 4.0, 0.0, 3.0, -2.0, 2.0, 1.0, 5.0, 0.0 ] );
	x = new Complex128Array( [ 1.0, 0.5, 0.0, 0.0, 2.0, -1.0, 0.0, 0.0, 3.0, 1.0 ] );
	y = new Complex128Array( [ 0.5, 1.0, 0.0, 0.0, 1.5, -0.5, 0.0, 0.0, 2.5, 0.0 ] );
	alpha = new Complex128( 1.0, 0.0 );

	zhpr2( 'upper', 3, alpha, x, 2, 0, y, 2, 0, ap, 1, 0 );

	rv = reinterpret( ap, 0 );
	assertArrayClose( rv, tc.AP, 1e-14, 'AP' );
});

test( 'zhpr2: zero elements in x and y', function t() {
	var alpha;
	var tc;
	var ap;
	var rv;
	var x;
	var y;

	tc = findCase( 'zero_elements' );
	ap = new Complex128Array( [ 2.0, 0.0, 1.0, 1.0, 4.0, 0.0, 3.0, -2.0, 2.0, 1.0, 5.0, 0.0 ] );
	x = new Complex128Array( [ 1.0, 0.5, 0.0, 0.0, 3.0, 1.0 ] );
	y = new Complex128Array( [ 0.5, 1.0, 0.0, 0.0, 2.5, 0.0 ] );
	alpha = new Complex128( 1.0, 0.0 );

	zhpr2( 'upper', 3, alpha, x, 1, 0, y, 1, 0, ap, 1, 0 );

	rv = reinterpret( ap, 0 );
	assertArrayClose( rv, tc.AP, 1e-14, 'AP' );
});

test( 'zhpr2: lower triangle with zero elements in x and y', function t() {
	var alpha;
	var ap;
	var rv;
	var x;
	var y;

	// Lower packed N=3: ap = [a11, a21, a31, a22, a32, a33]
	// x[1]=0, y[1]=0 => column 1: only diagonal forced real
	ap = new Complex128Array( [ 2.0, 0.5, 1.0, -1.0, 3.0, 2.0, 4.0, 0.3, 2.0, -1.0, 5.0, 0.1 ] );
	x = new Complex128Array( [ 1.0, 0.5, 0.0, 0.0, 3.0, 1.0 ] );
	y = new Complex128Array( [ 0.5, 1.0, 0.0, 0.0, 2.5, 0.0 ] );
	alpha = new Complex128( 1.0, 0.0 );

	zhpr2( 'lower', 3, alpha, x, 1, 0, y, 1, 0, ap, 1, 0 );

	rv = reinterpret( ap, 0 );

	// Column 0 (j=0): x[0]=(1,0.5), y[0]=(0.5,1) both nonzero
	// temp1 = alpha*conj(y[0]) = (1,0)*(0.5,-1) = (0.5,-1)
	// temp2 = conj(alpha*x[0]) = conj((1,0)*(1,0.5)) = conj(1,0.5) = (1,-0.5)
	// Diagonal (kk=0): real(AP[0]) + real(x[0]*temp1 + y[0]*temp2)
	//   x[0]*temp1 = (1,0.5)*(0.5,-1) = (0.5+0.5, -1+0.25) = (1.0, -0.75)
	//   y[0]*temp2 = (0.5,1)*(1,-0.5) = (0.5+0.5, -0.25+1) = (1.0, 0.75)
	//   sum real = 1.0+1.0 = 2.0, diag = 2.0+2.0 = 4.0, imag = 0
	assert.ok( Math.abs( rv[0] - 4.0 ) < 1e-14, 'ap[0,0] real' );
	assert.ok( Math.abs( rv[1] - 0.0 ) < 1e-14, 'ap[0,0] imag' );

	// Column 1 (j=1): x[1]=(0,0), y[1]=(0,0) => else branch, diagonal forced real
	// kk for column 1 = kk_prev + (N-0) = 0 + 3*2 = 6 (double index)
	// Diagonal at kk=6: just set imag to 0
	assert.ok( Math.abs( rv[7] - 0.0 ) < 1e-14, 'ap[1,1] imag forced to 0' );

	// Column 2 (j=2): x[2]=(3,1), y[2]=(2.5,0) both nonzero
	// Diagonal should be updated and imag=0
	assert.ok( Math.abs( rv[11] - 0.0 ) < 1e-14, 'ap[2,2] imag' );
});

test( 'zhpr2: lower triangle with non-unit stride', function t() {
	var alpha;
	var tc;
	var ap;
	var rv;
	var x;
	var y;

	tc = findCase( 'lower_stride_2' );
	ap = new Complex128Array( [ 2.0, 0.0, 1.0, -1.0, 3.0, 2.0, 4.0, 0.0, 2.0, -1.0, 5.0, 0.0 ] );
	x = new Complex128Array( [ 1.0, 0.5, 0.0, 0.0, 2.0, -1.0, 0.0, 0.0, 3.0, 1.0 ] );
	y = new Complex128Array( [ 0.5, 1.0, 0.0, 0.0, 1.5, -0.5, 0.0, 0.0, 2.5, 0.0 ] );
	alpha = new Complex128( 1.0, 0.0 );

	zhpr2( 'lower', 3, alpha, x, 2, 0, y, 2, 0, ap, 1, 0 );

	rv = reinterpret( ap, 0 );
	assertArrayClose( rv, tc.AP, 1e-14, 'AP' );
});
