
'use strict';

var Float64Array = require( '@stdlib/array/float64' ); // eslint-disable-line stdlib/require-globals
var dlaed6 = require( './../lib' );

var d = new Float64Array( [ 1.0, 3.0, 7.0 ] );
var z = new Float64Array( [ 0.3, 0.5, 0.8 ] );
var tau = new Float64Array( 1 );
var rho = 0.5;
var finit = rho + (z[0]/d[0]) + (z[1]/d[1]) + (z[2]/d[2]);

var info = dlaed6( 2, true, rho, d, z, finit, tau );
console.log( 'info:', info, 'tau:', tau[ 0 ] ); // eslint-disable-line no-console
