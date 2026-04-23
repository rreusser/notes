'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zgbcon = require( './zgbcon.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zgbcon, 'ndarray', ndarray );


// EXPORTS //

module.exports = zgbcon;
