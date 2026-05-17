
/* eslint-disable camelcase */

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dorgtsqr_row = require( './dorgtsqr_row.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dorgtsqr_row, 'ndarray', ndarray );


// EXPORTS //

module.exports = dorgtsqr_row;
