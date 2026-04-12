/* eslint-disable camelcase */

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dla_porcond = require( './dla_porcond.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dla_porcond, 'ndarray', ndarray );


// EXPORTS //

module.exports = dla_porcond;
