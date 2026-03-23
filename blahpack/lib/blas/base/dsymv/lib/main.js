

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var dsymv = require( './dsymv.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dsymv, 'ndarray', ndarray );


// EXPORTS //

module.exports = dsymv;
