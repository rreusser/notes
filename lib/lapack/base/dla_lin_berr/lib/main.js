
/* eslint-disable camelcase */

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dla_lin_berr = require( './dla_lin_berr.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dla_lin_berr, 'ndarray', ndarray );


// EXPORTS //

module.exports = dla_lin_berr;
