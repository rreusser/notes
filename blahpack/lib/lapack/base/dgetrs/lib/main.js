

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var dgetrs = require( './dgetrs.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dgetrs, 'ndarray', ndarray );


// EXPORTS //

module.exports = dgetrs;
