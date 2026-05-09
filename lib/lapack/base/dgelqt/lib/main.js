
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dgelqt = require( './dgelqt.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dgelqt, 'ndarray', ndarray );


// EXPORTS //

module.exports = dgelqt;
