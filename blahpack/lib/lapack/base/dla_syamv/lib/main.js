/* eslint-disable camelcase */

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dla_syamv = require( './dla_syamv.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dla_syamv, 'ndarray', ndarray );


// EXPORTS //

module.exports = dla_syamv;
