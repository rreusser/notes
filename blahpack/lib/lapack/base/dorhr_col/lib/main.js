
/* eslint-disable camelcase */

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dorhr_col = require( './dorhr_col.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dorhr_col, 'ndarray', ndarray );


// EXPORTS //

module.exports = dorhr_col;
