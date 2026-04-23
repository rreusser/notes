
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dsptrs = require( './dsptrs.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dsptrs, 'ndarray', ndarray );


// EXPORTS //

module.exports = dsptrs;
