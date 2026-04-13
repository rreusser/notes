
/* eslint-disable camelcase */

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dlarfb_gett = require( './dlarfb_gett.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dlarfb_gett, 'ndarray', ndarray );


// EXPORTS //

module.exports = dlarfb_gett;
