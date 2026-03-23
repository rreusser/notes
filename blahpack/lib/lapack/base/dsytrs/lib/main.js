

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var dsytrs = require( './dsytrs.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dsytrs, 'ndarray', ndarray );


// EXPORTS //

module.exports = dsytrs;
