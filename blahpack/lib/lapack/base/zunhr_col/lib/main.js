

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zunhr_col = require( './zunhr_col.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zunhr_col, 'ndarray', ndarray );


// EXPORTS //

module.exports = zunhr_col;
