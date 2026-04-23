'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dsyconv = require( './dsyconv.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dsyconv, 'ndarray', ndarray );


// EXPORTS //

module.exports = dsyconv;
