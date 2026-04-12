/* eslint-disable camelcase */

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dla_gercond = require( './dla_gercond.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dla_gercond, 'ndarray', ndarray );


// EXPORTS //

module.exports = dla_gercond;
