'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zlaqr2 = require( './zlaqr2.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zlaqr2, 'ndarray', ndarray );


// EXPORTS //

module.exports = zlaqr2;
