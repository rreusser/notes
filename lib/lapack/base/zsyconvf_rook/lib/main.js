/* eslint-disable camelcase */

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zsyconvf_rook = require( './zsyconvf_rook.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zsyconvf_rook, 'ndarray', ndarray );


// EXPORTS //

module.exports = zsyconvf_rook;
