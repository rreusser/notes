

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var dgelqf = require( './dgelqf.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dgelqf, 'ndarray', ndarray );


// EXPORTS //

module.exports = dgelqf;
