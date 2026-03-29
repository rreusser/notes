'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zdotu = require( './zdotu.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zdotu, 'ndarray', ndarray );


// EXPORTS //

module.exports = zdotu;
