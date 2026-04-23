'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dsygs2 = require( './dsygs2.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dsygs2, 'ndarray', ndarray );


// EXPORTS //

module.exports = dsygs2;
