'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zgeev = require( './zgeev.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zgeev, 'ndarray', ndarray );


// EXPORTS //

module.exports = zgeev;
