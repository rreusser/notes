
/* eslint-disable camelcase */

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zlarfb_gett = require( './zlarfb_gett.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zlarfb_gett, 'ndarray', ndarray );


// EXPORTS //

module.exports = zlarfb_gett;
