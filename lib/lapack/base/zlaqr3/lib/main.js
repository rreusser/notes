'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zlaqr3 = require( './zlaqr3.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zlaqr3, 'ndarray', ndarray );


// EXPORTS //

module.exports = zlaqr3;
