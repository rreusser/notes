'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zlaqr0 = require( './zlaqr0.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zlaqr0, 'ndarray', ndarray );


// EXPORTS //

module.exports = zlaqr0;
