/* eslint-disable camelcase */

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dla_geamv = require( './dla_geamv.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dla_geamv, 'ndarray', ndarray );


// EXPORTS //

module.exports = dla_geamv;
