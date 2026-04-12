/* eslint-disable camelcase */

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dla_gbamv = require( './dla_gbamv.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dla_gbamv, 'ndarray', ndarray );


// EXPORTS //

module.exports = dla_gbamv;
