

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var dgesv = require( './dgesv.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dgesv, 'ndarray', ndarray );


// EXPORTS //

module.exports = dgesv;
