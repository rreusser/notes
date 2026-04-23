'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zlaqr4 = require( './zlaqr4.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zlaqr4, 'ndarray', ndarray );


// EXPORTS //

module.exports = zlaqr4;
