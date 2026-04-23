'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dtrttf = require( './dtrttf.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dtrttf, 'ndarray', ndarray );


// EXPORTS //

module.exports = dtrttf;
