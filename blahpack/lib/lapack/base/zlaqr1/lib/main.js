'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zlaqr1 = require( './zlaqr1.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zlaqr1, 'ndarray', ndarray );


// EXPORTS //

module.exports = zlaqr1;
