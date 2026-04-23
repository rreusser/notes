'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zlaqr5 = require( './zlaqr5.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zlaqr5, 'ndarray', ndarray );


// EXPORTS //

module.exports = zlaqr5;
