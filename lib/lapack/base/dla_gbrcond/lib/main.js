/* eslint-disable camelcase */

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dla_gbrcond = require( './dla_gbrcond.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dla_gbrcond, 'ndarray', ndarray );


// EXPORTS //

module.exports = dla_gbrcond;
