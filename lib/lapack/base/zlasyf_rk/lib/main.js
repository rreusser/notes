
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zlasyfRk = require( './zlasyf_rk.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zlasyfRk, 'ndarray', ndarray );


// EXPORTS //

module.exports = zlasyfRk;
