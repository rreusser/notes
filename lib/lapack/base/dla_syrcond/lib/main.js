/* eslint-disable camelcase */

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dla_syrcond = require( './dla_syrcond.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dla_syrcond, 'ndarray', ndarray );


// EXPORTS //

module.exports = dla_syrcond;
