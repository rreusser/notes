
/* eslint-disable camelcase */

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zungtsqr_row = require( './zungtsqr_row.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zungtsqr_row, 'ndarray', ndarray );


// EXPORTS //

module.exports = zungtsqr_row;
