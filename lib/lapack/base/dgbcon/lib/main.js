'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dgbcon = require( './dgbcon.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dgbcon, 'ndarray', ndarray );


// EXPORTS //

module.exports = dgbcon;
