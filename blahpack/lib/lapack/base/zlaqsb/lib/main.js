'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zlaqsb = require( './zlaqsb.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zlaqsb, 'ndarray', ndarray );


// EXPORTS //

module.exports = zlaqsb;
