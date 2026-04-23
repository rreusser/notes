

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dgeql2 = require( './dgeql2.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dgeql2, 'ndarray', ndarray );


// EXPORTS //

module.exports = dgeql2;
