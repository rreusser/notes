'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dsygst = require( './dsygst.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dsygst, 'ndarray', ndarray );


// EXPORTS //

module.exports = dsygst;
